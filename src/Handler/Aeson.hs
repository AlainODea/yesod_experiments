module Handler.Aeson where

import           Data.Maybe  (fromMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Import
import           MachineJSON

-- /aeson/ns1.alainodea.local?image_uuid=0084dad6-05c1-11e3-9476-8f8320925eea
getAesonR :: String -> Handler Value
getAesonR name =
  do
    imageUuidMaybe <- lookupGetParam "image_uuid"
    let imageUuid = fromMaybe "0084dad6-05c1-11e3-9476-8f8320925eea" imageUuidMaybe
    maxMemoryMaybe <- lookupGetParam "max_physical_memory"
    let maxMemory = maybe 512 ((read :: String -> Int) . T.unpack) maxMemoryMaybe
    aliasMaybe <- lookupGetParam "alias"
    let alias = fromMaybe "nameserver" aliasMaybe
    nicTagMaybe <- lookupGetParam "nic_tag"
    let nicTag = fromMaybe "external" nicTagMaybe
    pubKeyMaybe <- lookupGetParam "root_authorized_keys"
    let pubKey = fromMaybe "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCTLDnggGlVWHOrlbEpLP8ZCmUQrUwq13+x+e216gghxQi+IZiEfTpQWoFqz5opDVgNzRdBWfMcS6qhBK1YyIt/I+W6tZTsvR3Ncq6XS0EATdSFZWozpgrC5jFnR1fAAoAJCDqcUs7lMd2Fg5MJe3y9PTlcFP+WOxfN/3zwqd1kH0hCJGLZbS3LgB7IaxLQylLwa6MzOvsw/65s24whGfGCW6Fgh/tsC2W2wLzKix6qNg8ypoBYtBZqtx41quDDfg62bDX53CXgqDxiYtUvQGUA7jvALtig8ttAZyZdWbAIgKKwURZjQIFIMMPhpT6eHkSIQ3qXNkh2qvlOmvM4s2LJ alain.odea@gmail.com" pubKeyMaybe
    ipMaybe <- lookupGetParam "ip"
    let ipAddr = fromMaybe "192.168.2.1" ipMaybe
    return . machineJSON $ MachineSpec [
                 Brand "joyent"
                ,ImageUUID imageUuid
                ,MaxPhysicalMemory maxMemory
                ,Hostname $ T.pack hostname
                ,DNSDomain $ T.pack dnsDomain
                ,Resolvers [ip 208 67 222 222, ip 208 67 220 220]
                ,Alias alias
                ,NICs [NIC [NICTag nicTag, Gateway $ ip 192 168 2 1, IP $ ip 192 168 2 2, Netmask $ ip 255 255 255 0]]
                ,CustomerMetadata [RootAuthorizedKeys [RootAuthorizedKey pubKey]]
                ]
  where hostname = takeWhile (/='.') name
        dnsDomain = tail $ dropWhile (/='.') name
