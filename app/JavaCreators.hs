module JavaCreators where

import Java

identifier :: String -> Identifier
identifier name = Identifier { _idName = name }

emptyClass :: Identifier -> Class
emptyClass identifier =
  Class { _className = identifier
        , _classFields = []
        , _classMethods = []
        , _classVisibility = Public
        , _classExtends = Nothing
        , _classImplements = []
        , _classFinal = False
        }
