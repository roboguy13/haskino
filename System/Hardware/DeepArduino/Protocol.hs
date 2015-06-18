-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.Comm
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal representation of the firmata protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs      #-}

module System.Hardware.DeepArduino.Protocol(packageProcedure, packageQuery, unpackageSysEx, unpackageNonSysEx, parseQueryResult) where

import Data.Bits ((.|.), (.&.))
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.Map        as M

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Utils

-- | Wrap a sys-ex message to be sent to the board
sysEx :: SysExCmd -> [Word8] -> B.ByteString
sysEx cmd bs = B.pack $  firmataCmdVal START_SYSEX
                      :  sysExCmdVal cmd
                      :  bs
                      ++ [firmataCmdVal END_SYSEX]

-- | Wrap the start of a sys-ex message to be sent to the board (all except
-- |  the END_SYSEX byte)
startSysEx :: SysExCmd -> [Word8] -> B.ByteString
startSysEx cmd bs = B.pack $  firmataCmdVal START_SYSEX
                      :  sysExCmdVal cmd
                      :  bs

-- | Construct a non sys-ex message
nonSysEx :: FirmataCmd -> [Word8] -> B.ByteString
nonSysEx cmd bs = B.pack $ firmataCmdVal cmd : bs



-- | Package a request as a sequence of bytes to be sent to the board
-- using the Firmata protocol.
packageProcedure :: ArduinoConnection -> Procedure -> IO B.ByteString
packageProcedure c SystemReset = 
    return $ nonSysEx SYSTEM_RESET []
packageProcedure c (AnalogReport  p b) = do
    ipin <- getInternalPin c p
    return $ nonSysEx (REPORT_ANALOG_PIN ipin) [if b then 1 else 0]
packageProcedure c (DigitalPortReport p b) = 
    return $ nonSysEx (REPORT_DIGITAL_PORT p) [if b then 1 else 0]
packageProcedure c (DigitalReport p b) = do
    ipin <- getInternalPin c p
    return $ nonSysEx (REPORT_DIGITAL_PORT $ pinPort ipin) [if b then 1 else 0]
packageProcedure c (SetPinMode p m) = do
    ipin <- getInternalPin c p
    return $ nonSysEx SET_PIN_MODE [fromIntegral $ pinNo ipin, fromIntegral $ fromEnum m]
packageProcedure c (DigitalPortWrite p l m) = 
    return $ nonSysEx (DIGITAL_MESSAGE p) [l, m]
packageProcedure c (DigitalWrite p b)  = do
    ipin <- getInternalPin c p
    return $ nonSysEx SET_DIGITAL_PIN_VALUE [fromIntegral $ pinNo ipin, if b then 1 else 0]
packageProcedure c (AnalogWrite p l m) = do
    ipin <- getInternalPin c p
    return $ nonSysEx (ANALOG_MESSAGE ipin) [l, m]
packageProcedure c (AnalogExtendedWrite p w8s) = do
    ipin <- getInternalPin c p
    return $ sysEx EXTENDED_ANALOG ([fromIntegral $ pinNo ipin] ++ (arduinoEncodedL w8s))
packageProcedure c (SamplingInterval l m) =
    return $ sysEx SAMPLING_INTERVAL [l, m]
packageProcedure c (I2CWrite sa w8s) = 
    return $ sysEx I2C_REQUEST ((packageI2c True sa Nothing 0) ++
                               ( concatMap toArduinoBytes w8s)) 
packageProcedure c (I2CConfig d) = 
    return $ sysEx I2C_CONFIG (word16ToArduinoBytes d)
packageProcedure c (StepperConfig dev TwoWire d sr p1 p2 _ _) = do
    ipin1 <-  getInternalPin c p1 
    ipin2 <-  getInternalPin c p2
    let pn1 = fromIntegral $ pinNo ipin1
        pn2 = fromIntegral $ pinNo ipin2
    return $ sysEx STEPPER_DATA ([stepperCmdVal CONFIG_STEPPER,dev,((stepDelayVal d) .|. 0x02)] ++ (word16ToArduinoBytes sr) ++ [pn1,pn2])
packageProcedure c (StepperConfig dev FourWire d sr p1 p2 (Just p3) (Just p4)) = do 
    ipin1 <-  getInternalPin c p1 
    ipin2 <-  getInternalPin c p2
    ipin3 <-  getInternalPin c p3 
    ipin4 <-  getInternalPin c p4
    let pn1 = fromIntegral $ pinNo ipin1
        pn2 = fromIntegral $ pinNo ipin2
        pn3 = fromIntegral $ pinNo ipin3
        pn4 = fromIntegral $ pinNo ipin4
    return $ sysEx STEPPER_DATA ([stepperCmdVal CONFIG_STEPPER,dev,((stepDelayVal d) .|. 0x04)] ++ (word16ToArduinoBytes sr) ++ [pn1,pn2,pn3,pn4])
packageProcedure c (StepperConfig _ FourWire _ _ _ _ _ _) = 
    runDie c "DeepArduino: FourWire steppers require specification of 4 pins for config"  []
packageProcedure c (StepperConfig dev StepDir d sr dp sp _ _) = do
    dipin <-  getInternalPin c dp 
    sipin <-  getInternalPin c sp
    let pnd = fromIntegral $ pinNo dipin
        pns = fromIntegral $ pinNo sipin
    return $ sysEx STEPPER_DATA ([stepperCmdVal CONFIG_STEPPER,dev,((stepDelayVal d) .|. 0x01)] ++ (word16ToArduinoBytes sr) ++ [pnd,pns])
packageProcedure c (StepperStep dev sd ns sp (Just ac)) =
    return $ sysEx STEPPER_DATA ([stepperCmdVal STEP_STEPPER,dev,(stepDirVal sd)] ++ (word32To3ArduinoBytes ns) ++ (word16ToArduinoBytes sp) ++ (intTo4ArduinoBytes ac))
packageProcedure c (StepperStep dev sd ns sp Nothing) =
    return $ sysEx STEPPER_DATA ([stepperCmdVal STEP_STEPPER,dev,(stepDirVal sd)] ++ (word32To3ArduinoBytes ns) ++ (word16ToArduinoBytes sp))
packageProcedure c (ServoConfig p min max)  = do
    ipin <- getInternalPin c p
    return $ sysEx SERVO_CONFIG ([fromIntegral $ pinNo ipin] ++ (word16ToArduinoBytes min) ++ (word16ToArduinoBytes max))
packageProcedure c (DeleteTask tid)         = 
    return $ sysEx SCHEDULER_DATA [schedulerCmdVal DELETE_TASK, tid]
packageProcedure c (Delay tt)               = 
    return $ sysEx SCHEDULER_DATA ([schedulerCmdVal DELAY_TASK] ++ (word32ToArduinoBytes tt))
packageProcedure c (ScheduleTask tid tt)    = 
    return $ sysEx SCHEDULER_DATA ([schedulerCmdVal SCHEDULE_TASK, tid] ++ (word32ToArduinoBytes tt))
packageProcedure c (CreateTask tid m)       = do
    td <- packageTaskData c m
    let taskSize = fromIntegral (B.length td)
    return $ sysEx SCHEDULER_DATA ([schedulerCmdVal CREATE_TASK, tid] ++ 
                                    (word16ToArduinoBytes taskSize))
                                    `B.append` (startSysEx SCHEDULER_DATA [schedulerCmdVal ADD_TO_TASK, tid])
                                    `B.append` (arduinoEncoded td)
                                    `B.append` (B.singleton (firmataCmdVal END_SYSEX))

packageTaskData :: ArduinoConnection -> Arduino a -> IO B.ByteString
packageTaskData conn commands =
      packageTaskData' conn commands B.empty
  where
      packBind :: ArduinoConnection -> Arduino a -> (a -> Arduino b) -> IO B.ByteString -> IO B.ByteString
      packBind c (Return a)      k cmds = do
          cs <- cmds
          packageTaskData' c (k a) cs
      packBind c (Bind m k1)    k2 cmds = packBind c m (\ r -> Bind (k1 r) k2) cmds
      packBind c (Procedure cmd) k cmds = do
          proc <- packageProcedure c cmd
          cs <- cmds
          packageTaskData' c (k ()) (B.append cs proc)
      packBind c (Local local)   k cmds = packLocal c local k cmds
      packBind c (Query query)   k cmds = packQuery c query k cmds

      -- For sending as part of a Scheduler task, locals make no sense.  
      -- Queries will work, but receiving them is problematic at the moment.
      -- Instead of signalling an error, at this point they are just ignored.
      packLocal :: ArduinoConnection -> Local a -> (a -> Arduino b) -> IO B.ByteString -> IO B.ByteString
      packLocal c (AnalogRead _) k cmds = do
          cs <- cmds 
          packageTaskData' c (k 0) cs
      packLocal c (DigitalPortRead _) k cmds = do
          cs <- cmds 
          packageTaskData' c (k 0) cs
      packLocal c (DigitalRead _) k cmds = do
          cs <- cmds 
          packageTaskData' c (k False) cs

      packQuery :: ArduinoConnection -> Query a -> (a -> Arduino b) -> IO B.ByteString -> IO B.ByteString
      packQuery c QueryFirmware k cmds = do 
          cs <- cmds           
          packageTaskData' c (k (0,0,[])) cs
      packQuery c CapabilityQuery k cmds = do
          cs <- cmds
          packageTaskData' c (k (BoardCapabilities M.empty)) cs
      packQuery c AnalogMappingQuery k cmds = do
          cs <- cmds 
          packageTaskData' c (k ([])) cs
      packQuery c (Pulse _ _ _ _) k cmds = do
          cs <- cmds 
          packageTaskData' c (k 0) cs
      packQuery c QueryAllTasks k cmds = do
          cs <- cmds
          packageTaskData' c (k ([])) cs
      packQuery c (QueryTask _) k cmds = do
          cs <- cmds
          packageTaskData' c (k (0,0,0,0,[])) cs

      packageTaskData' :: ArduinoConnection -> Arduino a -> B.ByteString -> IO B.ByteString
      packageTaskData' c (Bind m k) cmds = packBind c m k (return cmds)
      packageTaskData' c (Return a) cmds = return cmds
      packageTaskData' c cmd        cmds = packBind c cmd Return (return cmds)

packageQuery :: Query a -> B.ByteString
packageQuery QueryFirmware            = sysEx    REPORT_FIRMWARE         []
packageQuery CapabilityQuery          = sysEx    CAPABILITY_QUERY        []
packageQuery AnalogMappingQuery       = sysEx    ANALOG_MAPPING_QUERY    []
packageQuery (Pulse p b dur to)       = sysEx    PULSE                   ([fromIntegral (pinNo p), if b then 1 else 0] ++ concatMap toArduinoBytes (word32ToBytes dur ++ word32ToBytes to))
packageQuery (I2CRead sa sr cnt)      = sysEx    I2C_REQUEST  (packageI2c False sa sr cnt)
packageQuery QueryAllTasks            = sysEx    SCHEDULER_DATA [schedulerCmdVal QUERY_ALL_TASKS]
packageQuery (QueryTask tid)          = sysEx    SCHEDULER_DATA [schedulerCmdVal QUERY_TASK, tid]

packageI2c :: Bool -> SlaveAddress -> Maybe SlaveRegister -> Word8 -> [Word8]
packageI2c w sa sr cnt = [addrBytes !! 0, commandByte] ++ slaveBytes ++ countByte
  where
    addrBytes = toArduinoBytes sa
    commandByte = writeByte .|. (addrBytes !! 1)
    writeByte = if w then 0x00 else 0x08
    countByte = if w then [] else toArduinoBytes cnt
    slaveBytes = case sr of 
                    Nothing -> []
                    Just r  -> toArduinoBytes r

-- | Unpackage a SysEx response
unpackageSysEx :: [Word8] -> Response
unpackageSysEx []              = Unimplemented (Just "<EMPTY-SYSEX-CMD>") []
unpackageSysEx (cmdWord:args)
  | Right cmd <- getSysExCommand cmdWord
  = case (cmd, args) of
      (REPORT_FIRMWARE, majV : minV : rest)  -> Firmware majV minV (getString rest)
      (CAPABILITY_RESPONSE, bs)              -> Capabilities (getCapabilities bs)
      (ANALOG_MAPPING_RESPONSE, bs)          -> AnalogMapping bs
      (PULSE, xs) | length xs == 10          -> let [p, a, b, c, d] = fromArduinoBytes xs in PulseResponse (InternalPin p) (bytesToWord32 (a, b, c, d))
      (STRING_DATA, rest)                    -> StringMessage (getString rest)
      (I2C_REPLY, xs)                        -> let (sa:sr:idata) = fromArduinoBytes xs in I2CReply sa sr idata
      (SCHEDULER_DATA, srWord : ts) | Right sr <- getSchedulerReply srWord
        -> case sr of 
          QUERY_ALL_TASKS_REPLY              -> QueryAllTasksReply ts
          QUERY_TASK_REPLY | length ts == 1  -> QueryTaskReply (ts !! 0) 0 0 0 []
          -- TBD Fix reply decode
          QUERY_TASK_REPLY | length ts >= 11 -> let tt0:tt1:tt2:tt3:tl0:tl1:tp0:tp1:td = arduinoDecoded (tail ts)
                                                in QueryTaskReply (head ts) (bytesToWord32 (tt3,tt2,tt1,tt0)) (bytesToWord16 (tl1,tl0)) (bytesToWord16 (tp1,tp0)) td
          ERROR_FIRMATA_TASK_REPLY | length ts == 1  -> ErrorTaskReply (ts !! 0) 0 0 0 []
          -- TBD Fix reply decode
          ERROR_FIRMATA_TASK_REPLY | length ts >= 11 -> let tt0:tt1:tt2:tt3:tl0:tl1:tp0:tp1:td = arduinoDecoded (tail ts)
                                                in ErrorTaskReply (head ts) (bytesToWord32 (tt3,tt2,tt1,tt0)) (bytesToWord16 (tl1,tl0)) (bytesToWord16 (tp1,tp0)) td
      _                                      -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries
parseQueryResult :: Query a -> Response -> Maybe a
parseQueryResult QueryFirmware (Firmware wa wb s) = Just (wa,wb,s)
parseQueryResult CapabilityQuery (Capabilities bc) = Just bc
parseQueryResult AnalogMappingQuery (AnalogMapping ms) = Just ms
parseQueryResult (Pulse p b dur to) (PulseResponse p2 w) = Just w
parseQueryResult (I2CRead saq srq cnt) (I2CReply sar srr ds) = Just ds
parseQueryResult QueryAllTasks (QueryAllTasksReply ts) = Just ts
parseQueryResult (QueryTask tid) (QueryTaskReply tid' tt tl tp ws) = Just (tid',tt,tl,tp,ws)
parseQueryResult q r = Nothing

getCapabilities :: [Word8] -> BoardCapabilities
getCapabilities bs = BoardCapabilities $ M.fromList $ zipWith (\p c -> (p, PinCapabilities{analogPinNumber = Nothing, allowedModes = c}))
                                                              (map InternalPin [(0::Word8)..]) (map pinCaps (chunk bs))
  where chunk xs = case break (== 0x7f) xs of
                     ([], [])         -> []
                     (cur, 0x7f:rest) -> cur : chunk rest
                     _                -> [xs]
        pinCaps (x:y:rest) = (toEnum (fromIntegral x), y) : pinCaps rest
        pinCaps _          = []

-- | Unpackage a Non-SysEx response
unpackageNonSysEx :: (Int -> IO [Word8]) -> FirmataCmd -> IO Response
unpackageNonSysEx getBytes c = grab c
 where unimplemented n = Unimplemented (Just (show c)) `fmap` getBytes n
       grab (ANALOG_MESSAGE       p)    = getBytes 2 >>= \[l, h] -> return (AnalogMessage  p l h)
       grab (DIGITAL_MESSAGE      p)    = getBytes 2 >>= \[l, h] -> return (DigitalMessage p l h)
       -- we should never see any of the following since they are "request" codes
       grab (REPORT_ANALOG_PIN   _pin)  = unimplemented 1
       grab (REPORT_DIGITAL_PORT _port) = unimplemented 1
       grab START_SYSEX                 = unimplemented 0
       grab SET_PIN_MODE                = unimplemented 2
       grab END_SYSEX                   = unimplemented 0
       grab PROTOCOL_VERSION            = unimplemented 2
       grab SYSTEM_RESET                = unimplemented 0
