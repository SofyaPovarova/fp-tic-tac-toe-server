{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}

module Routes where

import Data.Aeson
import Game
import GHC.Generics (Generic)
import Models
import Servant
import Servant.Auth.Server
import Control.Monad.IO.Class (liftIO)

type CookieHeaders = 
  Headers '[ Header "Set-Cookie" SetCookie
           , Header "Set-Cookie" SetCookie]
             NoContent
                                
data User = User { name :: String, email :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Login = Login { username :: String, password :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

type Protected =
  "field" :> Get '[JSON] Field
  :<|> "user" :> Get '[JSON] String

type Unprotected =
  "register"
     :> ReqBody '[JSON] Login
     :> Post '[JSON] CookieHeaders
  :<|> "login" 
     :> ReqBody '[JSON] Login 
     :> Post '[JSON] CookieHeaders
     
type API auths = (Servant.Auth.Server.Auth auths User :> Protected) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings =
  protected :<|> unprotected cookieSettings jwtSettings


protected :: Servant.Auth.Server.AuthResult User -> Server Protected
protected (Servant.Auth.Server.Authenticated user) =
  fieldHandler :<|> userHandler user
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cookieSettings jwtSettings = 
  registrationHandler :<|> (loginHandler cookieSettings jwtSettings)
  

fieldHandler :: Handler Field
fieldHandler = return (emptyField 3)

userHandler :: User -> Handler String
userHandler = return . show

registrationHandler :: Login -> Handler CookieHeaders
registrationHandler _ = undefined

loginHandler :: CookieSettings -> JWTSettings -> Login -> Handler CookieHeaders
loginHandler cookieSettings jwtSettings _ = do
   let usr = User "Ali Baba" "ali@email.com"
   mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return $ applyCookies NoContent
