module Hasql.Private.PTI where

import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Private.Prelude hiding (bool)

-- | A Postgresql type info
data PTI = PTI {ptiOID :: !OID, ptiArrayOID :: !(Maybe OID)}

-- | A Word32 and a LibPQ representation of an OID
data OID = OID {oidWord32 :: !Word32, oidPQ :: !LibPQ.Oid, oidFormat :: !LibPQ.Format}

mkOID :: LibPQ.Format -> Word32 -> OID
mkOID format x =
  OID x ((LibPQ.Oid . fromIntegral) x) format

mkPTI :: LibPQ.Format -> Word32 -> Maybe Word32 -> PTI
mkPTI format oid arrayOID =
  PTI (mkOID format oid) (fmap (mkOID format) arrayOID)

-- * Constants

abstime = mkPTI LibPQ.Binary 702 (Just 1023)

aclitem = mkPTI LibPQ.Binary 1033 (Just 1034)

bit = mkPTI LibPQ.Binary 1560 (Just 1561)

bool = mkPTI LibPQ.Binary 16 (Just 1000)

box = mkPTI LibPQ.Binary 603 (Just 1020)

bpchar = mkPTI LibPQ.Binary 1042 (Just 1014)

bytea = mkPTI LibPQ.Binary 17 (Just 1001)

char = mkPTI LibPQ.Binary 18 (Just 1002)

cid = mkPTI LibPQ.Binary 29 (Just 1012)

cidr = mkPTI LibPQ.Binary 650 (Just 651)

circle = mkPTI LibPQ.Binary 718 (Just 719)

cstring = mkPTI LibPQ.Binary 2275 (Just 1263)

date = mkPTI LibPQ.Binary 1082 (Just 1182)

daterange = mkPTI LibPQ.Binary 3912 (Just 3913)

float4 = mkPTI LibPQ.Binary 700 (Just 1021)

float8 = mkPTI LibPQ.Binary 701 (Just 1022)

gtsvector = mkPTI LibPQ.Binary 3642 (Just 3644)

inet = mkPTI LibPQ.Binary 869 (Just 1041)

int2 = mkPTI LibPQ.Binary 21 (Just 1005)

int2vector = mkPTI LibPQ.Binary 22 (Just 1006)

int4 = mkPTI LibPQ.Binary 23 (Just 1007)

int4range = mkPTI LibPQ.Binary 3904 (Just 3905)

int8 = mkPTI LibPQ.Binary 20 (Just 1016)

int8range = mkPTI LibPQ.Binary 3926 (Just 3927)

interval = mkPTI LibPQ.Binary 1186 (Just 1187)

json = mkPTI LibPQ.Binary 114 (Just 199)

jsonb = mkPTI LibPQ.Binary 3802 (Just 3807)

line = mkPTI LibPQ.Binary 628 (Just 629)

lseg = mkPTI LibPQ.Binary 601 (Just 1018)

macaddr = mkPTI LibPQ.Binary 829 (Just 1040)

money = mkPTI LibPQ.Binary 790 (Just 791)

name = mkPTI LibPQ.Binary 19 (Just 1003)

numeric = mkPTI LibPQ.Binary 1700 (Just 1231)

numrange = mkPTI LibPQ.Binary 3906 (Just 3907)

oid = mkPTI LibPQ.Binary 26 (Just 1028)

oidvector = mkPTI LibPQ.Binary 30 (Just 1013)

path = mkPTI LibPQ.Binary 602 (Just 1019)

point = mkPTI LibPQ.Binary 600 (Just 1017)

polygon = mkPTI LibPQ.Binary 604 (Just 1027)

record = mkPTI LibPQ.Binary 2249 (Just 2287)

refcursor = mkPTI LibPQ.Binary 1790 (Just 2201)

regclass = mkPTI LibPQ.Binary 2205 (Just 2210)

regconfig = mkPTI LibPQ.Binary 3734 (Just 3735)

regdictionary = mkPTI LibPQ.Binary 3769 (Just 3770)

regoper = mkPTI LibPQ.Binary 2203 (Just 2208)

regoperator = mkPTI LibPQ.Binary 2204 (Just 2209)

regproc = mkPTI LibPQ.Binary 24 (Just 1008)

regprocedure = mkPTI LibPQ.Binary 2202 (Just 2207)

regtype = mkPTI LibPQ.Binary 2206 (Just 2211)

reltime = mkPTI LibPQ.Binary 703 (Just 1024)

text = mkPTI LibPQ.Binary 25 (Just 1009)

tid = mkPTI LibPQ.Binary 27 (Just 1010)

time = mkPTI LibPQ.Binary 1083 (Just 1183)

timestamp = mkPTI LibPQ.Binary 1114 (Just 1115)

timestamptz = mkPTI LibPQ.Binary 1184 (Just 1185)

timetz = mkPTI LibPQ.Binary 1266 (Just 1270)

tinterval = mkPTI LibPQ.Binary 704 (Just 1025)

tsquery = mkPTI LibPQ.Binary 3615 (Just 3645)

tsrange = mkPTI LibPQ.Binary 3908 (Just 3909)

tstzrange = mkPTI LibPQ.Binary 3910 (Just 3911)

tsvector = mkPTI LibPQ.Binary 3614 (Just 3643)

txid_snapshot = mkPTI LibPQ.Binary 2970 (Just 2949)

unknown = mkPTI LibPQ.Text 705 (Just 705)

uuid = mkPTI LibPQ.Binary 2950 (Just 2951)

varbit = mkPTI LibPQ.Binary 1562 (Just 1563)

varchar = mkPTI LibPQ.Binary 1043 (Just 1015)

void = mkPTI LibPQ.Binary 2278 Nothing

xid = mkPTI LibPQ.Binary 28 (Just 1011)

xml = mkPTI LibPQ.Binary 142 (Just 143)
