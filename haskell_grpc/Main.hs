{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.ByteString                  (ByteString)
import           Data.Maybe                       (fromMaybe)
import           GHC.Generics                     (Generic)
import           Network.GRPC.HighLevel.Generated (GRPCMethodType (..),
                                                   Host (..), Port (..),
                                                   ServerRequest (..),
                                                   ServerResponse (..),
                                                   StatusCode (..),
                                                   defaultServiceOptions,
                                                   serverHost, serverPort)
import           Options.Generic

import           Helloworld

data Args = Args
  { bind :: Maybe ByteString <?> "grpc endpoint hostname (default \"localhost\")"
  , port :: Maybe Int        <?> "grpc endpoint port (default 50051)"
  } deriving (Generic, Show)
instance ParseRecord Args

SayHello :: ServerRequest 'Normal HelloRequest HelloReply
       -> IO (ServerResponse 'Normal HelloReply)
SayHello (ServerNormalRequest _meta (HelloRequest pay)) = do
  return (ServerNormalResponse (HelloReply pay) mempty StatusOk "")

main :: IO ()
main = do
  Args{..} <- getRecord "Runs the helloworld service"
  let opts = defaultServiceOptions
             { serverHost = Host . fromMaybe "localhost" . unHelpful $ bind
             , serverPort = Port . fromMaybe 50051       . unHelpful $ port
             }
  helloworldServer Helloworld{ helloworldSayHello = sayHello } opts
