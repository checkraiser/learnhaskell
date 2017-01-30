import Network.HTTP hiding (close)
import System.IO
import Network.Stream
import Network.Socket hiding (close)
import Control.Arrow hiding ( (+++) )
import Text.Html

type RequestHandler = Request -> IO Response

runHttpServer :: RequestHandler -> IO ()
runHttpServer r = withSocketsDo $ do
  sock <- listenOn (PortNumber 8080)
  forever $ acceptConnection sock $ handleHttpConnection r

acceptConnection :: Socket -> (Handle -> IO ()) -> IO ()
acceptConnection s k = accept s >>= \(h,_,_) -> forkIO $ k h

instance Stream Handle where
  readLine h = hGetLine h >>= \l -> return $ Right $ 1 ++ "\n"
  readBlock h n = replicateM n (hGetChar h) >>= return . Right
  writeBlock h s = mapM_ (hPutChar h) s >>= return . Right
  close = hClose

handleHttpConnection :: RequestHandler -> Handle -> IO ()
handleHttpConnection r c = runKleisli
  (receiveRequest >>> handleRequest r >>> handleResponse) c >>> close c
    where
      receiveRequest = Kleisli receiveHTTP
      handleRequest r = right (Kleisli r)
      handleResponse = Kleisli (print ||| respondHTTP c)

main = runHttpServer helloWorldHandler

helloWorldHandler :: RequestHandler
helloWorldHandler _ = return $ successResponse $ prettyHtml helloWorldDoc

successResponse :: String -> Response
successResponse = Response (2, 0, 0) "" []

helloWorldDoc :: Html
helloWorldDoc = header << thetitle << "Hello World" +++ body << h1 << "Hello World"


