module IR
( Command (..)
, CommandBuffer (..)
, cmdBufInit
, nextCmd
, getCmd
, interpretTokens
) 
where

import qualified Parser as T (Token (..))
import Parser (Token, TokenStream)

data Command
    = Add Int
    | Seek Int
    | Print
    | Scan
    | Label Int
    | Jump Int
    deriving (Show, Eq)

data CommandBuffer = CommandBuffer
    { commandPointer :: Int
    , commands :: [Command]
    }
    deriving (Show)

cmdBufInit :: Int -> CommandBuffer
cmdBufInit start =
    CommandBuffer start []

nextCmd :: CommandBuffer -> CommandBuffer
nextCmd (CommandBuffer p cs) =
    CommandBuffer (p + 1) cs

getCmd :: CommandBuffer -> Command
getCmd (CommandBuffer p cs) =
    cs !! p

inc :: Command
inc = Add 1

dec :: Command
dec = Add (-1)

shr :: Command
shr = Seek 1

shl :: Command
shl = Seek (-1)

appendCommand :: Command -> CommandBuffer -> CommandBuffer
appendCommand c (CommandBuffer p []) = CommandBuffer p [c]
appendCommand c (CommandBuffer p cs) =
    let lastCommand = last cs
        rest = init cs
    in
        CommandBuffer (p + 1) (
            case (lastCommand, c) of
                (Add a, Add b) ->
                    rest ++ [Add (a + b)]
                (Seek a, Seek b) ->
                    rest ++ [Seek (a + b)]
                _ ->
                    cs ++ [c]
        )

appendCommands :: [Command] -> CommandBuffer -> CommandBuffer
appendCommands src dest =
    case src of
        (c:cs) -> appendCommands cs (appendCommand c dest)
        [] -> dest

-- ? This function seems a little clunky
interpretToken :: Int -> Token -> [Command]
interpretToken cmdPtr t =
    case t of
        T.Plus -> [inc]
        T.Minus -> [dec]
        T.Shr -> [shr]
        T.Shl -> [shl]
        T.Read -> [Scan]
        T.Write -> [Print]
        T.Loop ts -> [Label cmdPtr] ++ commands (interpretTokens ts cmdPtr) ++ [Jump cmdPtr]

appendToken :: Token -> CommandBuffer -> CommandBuffer
appendToken t cb =
    appendCommands (interpretToken cmdPtr t) cb
    where
        cmdPtr = commandPointer cb

appendTokens :: TokenStream -> CommandBuffer -> CommandBuffer
appendTokens ts cs =
    case ts of
        (t:ts') -> appendTokens ts' (appendToken t cs)
        [] -> cs

interpretTokens :: TokenStream -> Int -> CommandBuffer
interpretTokens ts start =
    CommandBuffer start cmds
    where
        cmds =
            commands (appendTokens ts $ cmdBufInit start)