module URL
  (
  ) where

import           Data.Text (Text)
import           System.IO (Handle)


type URLAddr  = Text

newtype Connection = Connection
                   {  runWithConn :: (URLAddr, ConnOptions) -> IO Handle
                   }

data ConnType = TCP
              | UDP
              deriving (Show)

data UseProxy = NoProxy
              | Proxy Text
              deriving Show

data TimeOut = NoTimeOut
             | TimeOut Integer
             deriving Show

data ConnOptions = ConnOptions
               { _conType      :: ConnType
               , _conProxy     :: UseProxy
               , _conTimeout   :: TimeOut
               , _conSpeed     :: Integer
               , _conCaching   :: Bool
               , _conKeepAlive :: Bool
               } deriving Show

connect :: URLAddr -> ConnOptions -> Connection
connect = undefined

connDefault :: ConnOptions
connDefault = ConnOptions
            { _conType = TCP
            , _conProxy = NoProxy
            , _conTimeout = NoTimeOut
            , _conSpeed = 0
            , _conCaching = False
            }
