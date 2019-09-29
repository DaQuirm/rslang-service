module API.Doc.ToSchema where

import Data.Swagger (ToSchema)
import Database.Selda (ID, RowID)

instance ToSchema RowID
instance ToSchema (ID a)
