import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

tryPack = L.pack [98..120]

tryCons = L.cons 56 $ L.empty

myCopyFile fileA fileB = do cont <- L.readFile fileA
			    L.writeFile fileB cont

