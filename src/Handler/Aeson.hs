module Handler.Aeson where

import Import
import MachineJSON

getAesonR :: String -> Handler Value
getAesonR i = return $
    machineJSON $ MachineSpec [
                 (Brand "joyent")
                ,(ImageUUID "0084dad6-05c1-11e3-9476-8f8320925eea")
                ,(MaxPhysicalMemory 512)
                ,(Hostname "ns1")
                ,(DNSDomain "alainodea.local")
                ,(Resolvers [(ip 208 67 222 222), (ip 208 67 220 220)])
                ,(Alias "nameserver")
                ,(NICs [NIC [NICTag "external", Gateway $ ip 192 168 2 1, IP $ ip 192 168 2 2, Netmask $ ip 255 255 255 0]])
                ,(CustomerMetadata [RootAuthorizedKeys $ [RootAuthorizedKey "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCTLDnggGlVWHOrlbEpLP8ZCmUQrUwq13+x+e216gghxQi+IZiEfTpQWoFqz5opDVgNzRdBWfMcS6qhBK1YyIt/I+W6tZTsvR3Ncq6XS0EATdSFZWozpgrC5jFnR1fAAoAJCDqcUs7lMd2Fg5MJe3y9PTlcFP+WOxfN/3zwqd1kH0hCJGLZbS3LgB7IaxLQylLwa6MzOvsw/65s24whGfGCW6Fgh/tsC2W2wLzKix6qNg8ypoBYtBZqtx41quDDfg62bDX53CXgqDxiYtUvQGUA7jvALtig8ttAZyZdWbAIgKKwURZjQIFIMMPhpT6eHkSIQ3qXNkh2qvlOmvM4s2LJ alain.odea@gmail.com"]])
                ]
