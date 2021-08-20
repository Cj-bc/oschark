{-# LANGUAGE OverloadedStrings #-}
-- Oschark -- a simple osc packet viewer
--    Copyright (C) 2021 Cj.bc-sd a.k.a. Cj-bc
--
--    This program is free software; you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation; either version 3 of the License, or 
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful, 
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
--    GNU General Public License for more details.
--    
--    You should have received a copy of the GNU General Public License
--    along with this program. If not, see <https://www.gnu.org/licenses/>.
module Main where
import Sound.OSC
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.List (intersperse)

import Options.Applicative

-- | All available options that this executable has
data ExecutableOptions = ExecutableOptions { address :: String
                                           , portNumber :: Int
                                           }
  
options = ExecutableOptions <$> argument str (metavar "Address")
                            <*> argument auto (metavar "PortNumber")

gplInfo = unlines [ "oschark Copyright (C) 2021 Cj.bc-sd a.k.a. Cj-bc"
                  , "This program comes with ABSOLUTELY NO WARRANTY; for details type `info gpl’."
                  , "This is free software, and you are welcome to redistribute it"
                  , "under certain conditions; type `info gpl’ for details."
                  ]

optionParser = info (options <**> helper)
               $ fullDesc <> progDesc "Show OSC packets/bundles"
               <> header gplInfo

type UDPRecv = ReaderT UDP IO  

fromShow :: Show a => a -> Text
fromShow = fromString . show

formatPacket :: Packet -> Text
formatPacket (Packet_Message m) = formatMessage m
formatPacket (Packet_Bundle b)  = formatBundle b

formatBundle :: Bundle -> Text
formatBundle (Bundle time msgs) = "[" <> (fromShow time) <> "] \n"<> (T.unlines $ fmap ( mappend "\t" . formatMessage ) msgs)

formatMessage :: Message -> Text
formatMessage (Message addr datums) = mconcat $ [fromShow addr, ": "]
                                      ++ (intersperse "," . fmap formatDatum) datums

formatDatum :: Datum -> Text
formatDatum (Int32 v) 		= fromShow v
formatDatum (Int64 v)		= fromShow v
formatDatum (Float v)		= fromShow v
formatDatum (Double v)		= fromShow v
formatDatum (ASCII_String v)	= fromShow v
formatDatum (Blob v)		= fromShow v
formatDatum (TimeStamp v)	= fromShow v
formatDatum (Midi v)		= fromShow v

  
main :: IO ()
main = do
  opts <- execParser optionParser
  putStrLn "App is up. Trying to capture bundles..."

  withTransport_ (udpServer (address opts) (portNumber opts))
    . forever $ recvPacket >>= liftIO . TI.putStrLn . formatPacket
