module Hasql.PTI where

import Hasql.Prelude hiding (bool)
import qualified Database.PostgreSQL.LibPQ as LibPQ


-- | A Postgresql type info
data PTI = PTI { ptiOID :: !OID, ptiArrayOID :: !(Maybe OID) }

-- | A Word32 and a LibPQ representation of an OID
data OID = OID { oidWord32 :: !Word32, oidPQ :: !LibPQ.Oid }

mkOID :: Word32 -> OID
mkOID x =
  OID x ((LibPQ.Oid . fromIntegral) x)

mkPTI :: Word32 -> Maybe Word32 -> PTI
mkPTI oid arrayOID =
  PTI (mkOID oid) (fmap mkOID arrayOID)


-- * Constants
-------------------------

abstime         = mkPTI 702  (Just 1023)
aclitem         = mkPTI 1033 (Just 1034)
bit             = mkPTI 1560 (Just 1561)
bool            = mkPTI 16   (Just 1000)
box             = mkPTI 603  (Just 1020)
bpchar          = mkPTI 1042 (Just 1014)
bytea           = mkPTI 17   (Just 1001)
char            = mkPTI 18   (Just 1002)
cid             = mkPTI 29   (Just 1012)
cidr            = mkPTI 650  (Just 651)
circle          = mkPTI 718  (Just 719)
cstring         = mkPTI 2275 (Just 1263)
date            = mkPTI 1082 (Just 1182)
daterange       = mkPTI 3912 (Just 3913)
float4          = mkPTI 700  (Just 1021)
float8          = mkPTI 701  (Just 1022)
gtsvector       = mkPTI 3642 (Just 3644)
inet            = mkPTI 869  (Just 1041)
int2            = mkPTI 21   (Just 1005)
int2vector      = mkPTI 22   (Just 1006)
int4            = mkPTI 23   (Just 1007)
int4range       = mkPTI 3904 (Just 3905)
int8            = mkPTI 20   (Just 1016)
int8range       = mkPTI 3926 (Just 3927)
interval        = mkPTI 1186 (Just 1187)
json            = mkPTI 114  (Just 199)
line            = mkPTI 628  (Just 629)
lseg            = mkPTI 601  (Just 1018)
macaddr         = mkPTI 829  (Just 1040)
money           = mkPTI 790  (Just 791)
name            = mkPTI 19   (Just 1003)
numeric         = mkPTI 1700 (Just 1231)
numrange        = mkPTI 3906 (Just 3907)
oid             = mkPTI 26   (Just 1028)
oidvector       = mkPTI 30   (Just 1013)
path            = mkPTI 602  (Just 1019)
point           = mkPTI 600  (Just 1017)
polygon         = mkPTI 604  (Just 1027)
record          = mkPTI 2249 (Just 2287)
refcursor       = mkPTI 1790 (Just 2201)
regclass        = mkPTI 2205 (Just 2210)
regconfig       = mkPTI 3734 (Just 3735)
regdictionary   = mkPTI 3769 (Just 3770)
regoper         = mkPTI 2203 (Just 2208)
regoperator     = mkPTI 2204 (Just 2209)
regproc         = mkPTI 24   (Just 1008)
regprocedure    = mkPTI 2202 (Just 2207)
regtype         = mkPTI 2206 (Just 2211)
reltime         = mkPTI 703  (Just 1024)
text            = mkPTI 25   (Just 1009)
tid             = mkPTI 27   (Just 1010)
time            = mkPTI 1083 (Just 1183)
timestamp       = mkPTI 1114 (Just 1115)
timestamptz     = mkPTI 1184 (Just 1185)
timetz          = mkPTI 1266 (Just 1270)
tinterval       = mkPTI 704  (Just 1025)
tsquery         = mkPTI 3615 (Just 3645)
tsrange         = mkPTI 3908 (Just 3909)
tstzrange       = mkPTI 3910 (Just 3911)
tsvector        = mkPTI 3614 (Just 3643)
txid_snapshot   = mkPTI 2970 (Just 2949)
unknown         = mkPTI 705  (Just 705)
uuid            = mkPTI 2950 (Just 2951)
varbit          = mkPTI 1562 (Just 1563)
varchar         = mkPTI 1043 (Just 1015)
void            = mkPTI 2278 Nothing
xid             = mkPTI 28   (Just 1011)
xml             = mkPTI 142  (Just 143)

