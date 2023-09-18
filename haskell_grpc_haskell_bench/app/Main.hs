{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Network.GRPC.HighLevel
import           Network.GRPC.HighLevel.Generated

import           Helloworld

main :: IO ()
main =
  greeterServer
    (Greeter{ greeterSayHello = sayHello })
    defaultServiceOptions{serverHost = "0.0.0.0", serverPort = 50051}

sayHello
  :: ServerRequest 'Normal HelloRequest HelloReply
  -> IO (ServerResponse 'Normal HelloReply)
sayHello (ServerNormalRequest _metadata HelloRequest{..}) = do
  return $ ServerNormalResponse (HelloReply helloRequestRequest) [] StatusOk ""
