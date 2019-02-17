module Java.Creators where

import Java.Types

identifier :: String -> Identifier
identifier name = Identifier { _idName = name }

emptyClass :: Identifier -> Class
emptyClass identifier =
  Class { _className = identifier
        , _classFields = []
        , _classMethods = []
        , _classConstructors = []
        , _classVisibility = Public
        , _classExtends = Nothing
        , _classImplements = []
        , _classFinal = False
        }
