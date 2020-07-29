module VM where

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

vmAdd :: Int -> VM -> IO VM
vmAdd n (VM b a cb) =
    return $ VM (doAt (+n) a b) a (nextCmd cb)

vmSeek :: Int -> VM -> IO VM
vmSeek n (VM b a cb) =
    return $ VM b (a + n) (nextCmd cb)

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

vmPrint :: VM -> IO VM
vmPrint (VM b a cb) = do
    putChar $ chr $ b !! a
    return (VM b a (nextCmd cb))

vmScan :: VM -> IO VM
vmScan (VM b a cb) = do
    c <- getChar 
    return $ VM (doAt (const $ ord c) a b) a (nextCmd cb)

exec :: VM -> IO VM
exec vm =
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
                    return $ VM bus addr (nextCmd cmdBuf)
            Print ->
                vmPrint vm
            Scan ->
                vmScan vm
            Label l ->
                if bus !! addr == 0 then 
                    vmBreak l vm 
                else
                    return $ VM bus addr (nextCmd cmdBuf)

vmRun :: VM -> IO ()
vmRun vm = do
    vm' <- exec vm
    if (cmdPtr vm') < (cmdCount vm') then 
        vmRun vm'
    else 
        return ()
    where 
        cmdPtr =
            commandPointer . commandBuffer
        cmdCount =
            length . commands . commandBuffer