-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Counter
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Demonstrates using two push-buttons to count up and down.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Counter where

import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino

-- | Two push-button switches, controlling a counter value. We will increment
-- the counter if the first one ('bUp') is pressed, and decrement the value if the
-- second one ('bDown') is pressed. We also have a led connected to pin 13 (either use
-- the internal or connect an external one), that we light up when the counter value
-- is 0.
--
-- Wiring is very simple: Up-button connected to pin 4, Down-button connected
-- to pin 2, and a led on pin 13.
--
--  <<http://http://github.com/ku-fpg/arduino-lab/raw/master/System/Hardware/Haskino/SamplePrograms/Schematics/Counter.png>>
counter :: IO ()
counter = do
       conn <- openArduino False "/dev/cu.usbmodem1421"
       send conn $ do
            setPinMode led   OUTPUT
            setPinMode bUp   INPUT
            setPinMode bDown INPUT
       update conn (0::Int)
 where bUp   = 4
       bDown = 2
       led   = 13
       update c curVal = do
                print curVal
                [up, down] <- send c $ do
                    digitalWrite led (curVal == 0)
                    waitAnyHigh [bUp, bDown]
                let newVal = case (up, down) of
                               (True,  True)  -> curVal    -- simultaneous press
                               (True,  False) -> curVal+1
                               (False, True)  -> curVal-1
                               (False, False) -> curVal    -- can't happen
                update c newVal
