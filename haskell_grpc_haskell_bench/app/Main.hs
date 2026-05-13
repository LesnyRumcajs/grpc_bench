{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Proto.API.Helloworld

sayHello :: Proto HelloRequest -> IO (Proto HelloReply)
sayHello req =
    pure $ defMessage & #response .~ (req ^. #request)

methods :: Methods IO (ProtobufMethodsOf Greeter)
methods =
      Method (mkNonStreaming sayHello)
    $ NoMoreMethods

main :: IO ()
main = runServerWithHandlers def config $ fromMethods methods
  where
    config :: ServerConfig
    config = ServerConfig
        { serverInsecure = Just (InsecureConfig (Just "0.0.0.0") 50051)
        , serverSecure   = Nothing
        }
