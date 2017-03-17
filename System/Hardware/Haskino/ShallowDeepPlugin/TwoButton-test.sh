#!/bin/sh
(cd ../../../..;ghc System/Hardware/Haskino/SamplePrograms/Rewrite/TwoButton.hs -package ghc -O2) && ../SamplePrograms/Rewrite/TwoButton
