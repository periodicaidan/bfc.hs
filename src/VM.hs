module VM 
( VM (..)
, vmInit 
, vmExec 
, vmRun 
) 
where

import Data.Char
import Data.List

import IR

data VM = VM
    { bus :: [Int]
    , addr :: Int
    , commandBuffer :: CommandBuffer
    }

doAt :: (a -> a) -> Int -> [a] -> [a]
doAt f i xs =
    let (rest, (affected : rest')) = splitAt i xs in
        rest ++ [f affected] ++ rest'

vmInit :: CommandBuffer -> VM
vmInit =
    VM (repeat 0) 0

vmStep :: VM -> VM 
vmStep (VM b a cb) = 
    VM b a (nextCmd cb)

vmAdd :: Int -> VM -> IO VM
vmAdd n (VM b a cb) =
    return . vmStep $ VM addToBus a cb
    where 
        addToBus = doAt (+n) a b

vmSeek :: Int -> VM -> IO VM
vmSeek n (VM b a cb) =
    return . vmStep $ VM b (a + n) cb

vmJump :: Int -> VM -> IO VM
vmJump l (VM b a cb) =
    let (CommandBuffer _ cs) = cb in
    return $ 
        case elemIndex (Label l) cs of
            Just i -> 
                VM b a (CommandBuffer (i + 1) cs)
            Nothing ->
                VM b a (nextCmd cb)

vmBreak :: Int -> VM -> IO VM 
vmBreak l (VM b a cb) =
    let (CommandBuffer _ cs) = cb in
    return $ 
        case elemIndex (Jump l) cs of
            Just i -> 
                VM b i (CommandBuffer (i + 1) cs)
            Nothing ->
                VM b a (nextCmd cb)

-- TODO: Buffer the print statements
vmPrint :: VM -> IO VM
vmPrint (VM b a cb) = do
    putChar . chr $ b !! a
    return (VM b a (nextCmd cb))

vmScan :: VM -> IO VM
vmScan (VM b a cb) = do
    c <- getChar 
    return . vmStep $ VM (writeBus c) a cb
    where 
        writeBus c = doAt (const $ ord c) a b

vmExec :: VM -> IO VM
vmExec vm =
    let (VM bus addr cmdBuf) = vm in
        case getCmd cmdBuf of
            Add n ->
                vmAdd n vm
            Seek n ->
                vmSeek n vm
            Jump l ->
                if bus !! addr /= 0 then 
                    vmJump l vm
                else 
                    return $ vmStep vm
            Print ->
                vmPrint vm
            Scan ->
                vmScan vm
            Label l ->
                if bus !! addr == 0 then 
                    vmBreak l vm 
                else
                    return $ vmStep vm

vmRun :: VM -> IO ()
vmRun vm = do
    vm' <- vmExec vm
    if (cmdPtr vm') < (cmdCount vm') then 
        vmRun vm'
    else 
        return ()
    where 
        cmdPtr =
            commandPointer . commandBuffer
        cmdCount =
            length . commands . commandBuffer