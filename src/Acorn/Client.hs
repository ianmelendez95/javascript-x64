module Acorn.Client where 

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_in, std_out),
      StdStream(CreatePipe) )
import GHC.IO.Handle ( hGetContents, hPutStr, hClose )
import Data.Aeson (Value (..), eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Either

import Acorn.Syntax

invokeAcornClientOutput :: String -> IO AcornOutput 
invokeAcornClientOutput js_source = either error id . eitherDecode <$> invokeAcornClientRaw js_source

invokeAcornClientJson :: String -> IO Value
invokeAcornClientJson js_source = either error id . eitherDecode <$> invokeAcornClientRaw js_source

invokeAcornClientRaw :: String -> IO B.ByteString 
invokeAcornClientRaw js_source = do create_result <- createProcess (proc "node" ["acorn-client/acorn-client.js"]){ std_in = CreatePipe, std_out = CreatePipe }
                                    case create_result of 
                                      (Just stdin, Just stdout, _, _) -> hPutStr stdin js_source >> hClose stdin >> B.hGetContents stdout
                                      _ -> error "Expecting stdin and stdout handles"

