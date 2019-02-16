module Focus where

import Java.Types (Element)

-- The first element represents the most detailled focus
-- Consecutive elements are higher up in the hierarchy
type Focus = [Element]

focusUp :: Focus -> Focus
focusUp = drop 1

focusDown :: Element -> Focus -> Focus
focusDown = (:)

focusRoot :: Focus -> Focus
focusRoot _ = []
