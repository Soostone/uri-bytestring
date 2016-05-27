module URI.ByteString.Normalization where


-------------------------------------------------------------------------------
import           URI.ByteString.Types
-------------------------------------------------------------------------------


rfcNormalize = normalizeByScheme .
               removeDotSegments .
               decodeRedundantPercentEncodings .
               upcasePercentEncodings .
               downcaseHost .
               downcaseScheme


normalizeByScheme = error "normalizeByScheme"

removeDotSegments = error "removeDotSegments"

decodeRedundantPercentEncodings = error "decodeRedundantPercentEncodings"

upcasePercentEncodings = error "upcasePercentEncodings"

downcaseHost = error "downcaseHost"

downcaseScheme = error "downcaseScheme"
