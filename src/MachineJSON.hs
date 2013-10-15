module MachineJSON
   ( module MachineJSON
   ) where

import           Data.Bits      (Bits, shiftL, shiftR, (.&.))
import           Data.String    (IsString (fromString))
import           Data.Text      (Text, intercalate, pack)
import qualified Data.Text.IO   as T
import           Data.Aeson.Types (ToJSON(toJSON), (.=), Value, object)
import           Network.Socket (HostAddress)

machineJSON :: MachineSpec -> Value
machineJSON (MachineSpec properties) =
    object $ map toPair properties
    where toPair (Brand x)             = "brand" .= x
          toPair (ImageUUID x)         = "image_uuid" .= x
          toPair (MaxPhysicalMemory x) = "max_physical_memory" .= x
          toPair (Hostname x)          = "hostname" .= x
          toPair (DNSDomain x)         = "dns_domain" .= x
          toPair (Resolvers x)         = "resolvers" .= x
          toPair (Alias x)             = "alias" .= x
          toPair (NICs x)              = "nics" .= x
          toPair (CustomerMetadata x)  = "customer_metadata" .= x

ip ::  Bits a => a -> a -> a -> a -> a
ip oct1 oct2 oct3 oct4 = oct1 `shiftL` 24 + oct2 `shiftL` 16 + oct3 `shiftL` 8 + oct4

byte :: Int -> IPAddress -> IPAddress
byte n x | n == 0 = x `shiftR` 24
         | n == 1 = (x .&. 255 `shiftL` 16) `shiftR` 16
         | n == 2 = (x .&. 255 `shiftL` 8) `shiftR` 8
         | n == 3 = (x .&. 255)
         | otherwise = 0

newtype MachineSpec = MachineSpec [MachineProp]
data MachineProp =
      Brand Text
    | ImageUUID Text
    | MaxPhysicalMemory Int
    | Hostname Text
    | DNSDomain Text
    | Resolvers [Resolver]
    | Alias Text
    | NICs [NIC]
    | CustomerMetadata [CustomerMetadataProp]

newtype Resolver = Resolver IPAddress deriving (ToJSON)

newtype NIC = NIC [NICProp]
data NICProp =
      NICTag Text
    | Gateway IPAddress
    | IP IPAddress
    | Netmask IPAddress

data CustomerMetadataProp =
      RootAuthorizedKeys [RootAuthorizedKey]
newtype RootAuthorizedKey = RootAuthorizedKey Text deriving (ToJSON)

newtype IPAddress = IPAddress HostAddress deriving (Num, Eq, Data.Bits.Bits)

instance Show IPAddress where
    show (IPAddress x) = show x

instance ToJSON NIC where
    toJSON (NIC props) = object $ map toPair props
      where toPair (NICTag nicTag)   = "nic_tag" .= nicTag
            toPair (Gateway gateway) = "gateway" .= gateway
            toPair (IP ip)           = "ip" .= ip
            toPair (Netmask netmask) = "netmask" .= netmask

instance ToJSON CustomerMetadataProp where
    toJSON (RootAuthorizedKeys xs) = object [
        "root_authorized_keys" .= intercalate "\n" (map unbox xs)
      ]
      where unbox (RootAuthorizedKey x) = x

instance ToJSON IPAddress where
    toJSON x = toJSON $ intercalate "." [showByte 0 x, showByte 1 x, showByte 2 x, showByte 3 x]
      where showByte n x = pack . show $ byte n x
