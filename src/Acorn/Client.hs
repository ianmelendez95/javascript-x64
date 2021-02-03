module Acorn.Client where 

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_in, std_out),
      StdStream(CreatePipe) )
import GHC.IO.Handle ( hGetContents, hPutStr, hClose )

invokeAcornClient :: String -> IO String
invokeAcornClient js_source = do create_result <- createProcess (proc "node" ["acorn-client/acorn-client.js"]){ std_in = CreatePipe, std_out = CreatePipe }
                                 case create_result of 
                                   (Just stdin, Just stdout, _, _) -> hPutStr stdin js_source >> hClose stdin >> hGetContents stdout
                                   _ -> error "Expecting stdin and stdout handles"