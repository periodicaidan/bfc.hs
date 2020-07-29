module VM where

import IR (Command (..), CommandBuffer (..))

data VM = VM
    { bus :: [Int]
    , addr :: Int
    , commandBuffer :: CommandBuffer
    }

vmInit :: CommandBuffer -> VM
vmInit =
    VM (repeat 0) 0

vmAdd :: Int -> VM -> VM
vmAdd n vm =
    undefined

vmSeek :: Int -> VM -> VM
vmSeek n vm =
    undefined

vmJump :: Int -> VM -> VM
vmJump l vm =
    undefined

exec :: VM -> VM
exec vm =
    let cmdBuf = commandBuffer vm in
    case commands cmdBuf !! commandPointer cmdBuf of
        Add n ->
            vmAdd n vm
        Seek n -> 
            vmSeek n vm
        Jump l ->
            vmJump l vm
        _ -> 
            vm