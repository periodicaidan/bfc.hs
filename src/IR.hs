module IR where

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

inc :: Command
inc = Add 1

dec :: Command
dec = Add (-1)

shr :: Command
shr = Seek 1

shl :: Command
shl = Seek (-1)

appendCommand :: Command -> [Command] -> [Command]
appendCommand c [] = [c]
appendCommand c cs =
    let lastCommand = last cs
        rest = init cs
    in
        case (lastCommand, c) of
            (Add a, Add b) ->
                rest ++ [Add (a + b)]
            (Seek a, Seek b) ->
                rest ++ [Seek (a + b)]
            _ ->
                cs ++ [c]

appendCommands :: [Command] -> [Command] -> [Command]
appendCommands src dest = 
    case src of 
        (c:cs) -> appendCommands cs (appendCommand c dest)
        [] -> dest 

interpretToken :: Token -> [Command]
interpretToken t =
    case t of
        T.Plus -> [inc]
        T.Minus -> [dec]
        T.Shr -> [shr]
        T.Shl -> [shl]
        T.Read -> [Scan]
        T.Write -> [Print]
        T.Loop ts -> [Label nth] ++ (ts >>= interpretToken) ++ [Jump nth]
        where 
            nth = 1

appendToken :: Token -> [Command] -> [Command]
appendToken t =
    appendCommands (interpretToken t)

appendTokens :: TokenStream -> [Command] -> [Command]
appendTokens ts cs =
    case ts of 
        (t:ts') -> appendTokens ts' (appendToken t cs)
        [] -> cs

interpretTokens :: TokenStream -> [Command]
interpretTokens ts =
    appendTokens ts []