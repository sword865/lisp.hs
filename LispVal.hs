{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LispVal where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Exception
import Control.Monad.Trans.Except
import Data.IORef
import Data.Typeable
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text as T

data LispVal = Atom T.Text
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | String T.Text
             | Bool Bool
             | Character Char
             | Number Integer
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [T.Text], vararg :: Maybe T.Text, body :: [LispVal], closure :: Env }
              deriving (Typeable)

showVal :: LispVal -> T.Text
showVal (Atom name) = name
showVal (List contents) = T.concat["(", unwordsList contents, ")"]
showVal (DottedList head tail) = T.concat ["(",  unwordsList head, " . ", showVal tail, ")"]
showVal (String contents) = T.concat ["\"", contents, "\""]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) =  T.pack [c]
showVal (Number number) = T.pack $ show number
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
    T.concat ["(lambda (", T.unwords args,
        case varargs of
            Nothing -> ""
            Just arg -> T.concat [" . ", arg], ") ...)"]


unwordsList :: [LispVal] -> T.Text
unwordsList content =  T.unwords $ showVal <$> content

instance Show LispVal where show = T.unpack . showVal

instance Exception LispException

data LispException = NumArgs Integer [LispVal]
                   | TypeMismatch T.Text LispVal
                   | Parser ParseError
                   | BadSpecialForm T.Text LispVal
                   | NotFunction T.Text T.Text
                   | UnboundVar T.Text T.Text
                   | Default T.Text

showError :: LispException -> T.Text
showError (NumArgs expected found)      = T.concat ["Expected ", T.pack $ show expected,
                                      " args; found values ", unwordsList found]
showError (TypeMismatch expected found) = T.concat ["Invalid type: expected ", expected,
                                      ", found ", showVal found]
showError (Parser parseErr)           = T.concat ["Parse error at ", T.pack $ show parseErr]
showError (BadSpecialForm message form) = T.concat [message, ": ", showVal form]
showError (NotFunction message func)    = T.concat [message, ": ", func]
showError (UnboundVar message varname)  = T.concat [message, ": ", varname]
showError (Default message)             = message

instance Show LispException where show = T.unpack . showError

type ThrowsError = Either LispException

trapError action = catchE action (return . showError)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type Env = IORef [(T.Text, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispException IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwE err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError T.Text -> IO T.Text
runIOThrows action = fmap extractValue (runExceptT (trapError action))

isBound :: Env -> T.Text -> IO Bool
isBound envRef var =  fmap (isJust . lookup var) (readIORef envRef)

getVar :: Env -> T.Text -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwE $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwE $ UnboundVar "Setting an unbound variable" var)
          (liftIO . flip writeIORef value)
          (lookup var env)
    return value

defineVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
        alreadyDefined <- liftIO $ isBound envRef var
        if alreadyDefined
           then setVar envRef var value
           else liftIO $ do
               valueRef <- newIORef value
               env <- readIORef envRef
               writeIORef envRef ((var, valueRef) : env)
               return value

bindVars :: Env -> [(T.Text, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do
              ref <- newIORef value
              return (var, ref)
