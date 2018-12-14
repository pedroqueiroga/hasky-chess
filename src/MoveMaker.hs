module MoveMaker
  ( possible_moves
  ) where

import Board

data TargetType = Friendly | Enemy | PassantEnemy | Outside | Empty deriving (Show, Eq)

get_type :: Board -> SquarePos -> Color -> TargetType
get_type board (row, col) player_c = let
    oob = (row < 0) || (col < 0) || (row > 7) || (col > 7)
    target = (board!!row)!!col
    target_t = case target of
        Nothing -> Empty
        Just (Piece pt clr) ->
            if (clr == player_c)
            then Friendly
            else if (pt == (Pawn Passant))
                then PassantEnemy
                else Enemy
    in
        if oob
        then Outside
        else target_t

place_piece :: Board -> SquarePos -> Square -> Board
place_piece board (row, col) sq = let
    (prevrows, relevantrow:nextrows) = splitAt row board
    (prevsqs, _:nextsqs) = splitAt col relevantrow
    in prevrows ++ [(prevsqs ++ (sq:nextsqs))] ++ nextrows

pawn_move :: Board -> SquarePos -> Square -> Color -> [Board]
pawn_move board (row, col) (Just (Piece (Pawn pt) _)) player_c = let

    cap pos = let
        target = get_type board pos player_c
        can_cap = Enemy == target || PassantEnemy == target
        in if(can_cap)
            then [place_piece board pos (Just $ Piece (Pawn Normal) player_c)]
            else []

    caps = foldl (++) [] $ map cap [(row + 1, col - 1), (row + 1, col + 1)]

    cap_passant (r, c) = let can_cap_passant = PassantEnemy == (get_type board (r - 1, c) player_c)
        in if(can_cap_passant)
            then let
                board1 = place_piece board (r, c) (Just $ Piece (Pawn Normal) player_c)
                in [place_piece board1 (r - 1, c) Nothing]
            else []

    caps_passant = foldl (++) [] $ map cap_passant [(row + 1, col - 1), (row + 1, col + 1)]

    can_forward = Empty == (get_type board (row + 1, col) player_c)
    move_forward = if(can_forward)
        then if (row + 1 == 7)
            then [place_piece board (row + 1, col) (Just $ Piece Queen player_c)]
            else [place_piece board (row + 1, col) (Just $ Piece (Pawn Normal) player_c)]
        else []

    can_forward_2 = pt == Starting && can_forward && Empty == (get_type board (row + 2, col) player_c)
    move_forward_2 = if(can_forward_2)
        then [place_piece board (row + 2, col) (Just $ Piece (Pawn MakeMePassant) player_c)]
        else []

    in caps ++ caps_passant ++ move_forward ++ move_forward_2
pawn_move _ _ _ _ = []

piece_attempt_move :: Board -> Square -> Color -> SquarePos -> [Board]
piece_attempt_move board square player_c pos = let
    target = get_type board pos player_c
    emptymove = Empty == target
    capturemove = Enemy == target || PassantEnemy == target
    in if (emptymove || capturemove)
        then [place_piece board pos square]
        else []

knight_move :: Board -> SquarePos -> Square -> Color -> [Board]
knight_move board (row, col) square player_c = let
    poses12 = [(row + x, col + y) | x <- [1,-1], y <- [2,-2]]
    poses21 = [(row + x, col + y) | x <- [2,-2], y <- [1,-1]]
    in foldl (++) [] $ map (piece_attempt_move board square player_c) (poses12 ++ poses21)

laser_beam :: Board -> SquarePos -> Square -> Color -> (SquarePos -> SquarePos) -> [Board]
laser_beam board pos square player_c laser =
    let newpos = laser pos
    in case (get_type board newpos player_c) of
        Friendly -> []
        Outside -> []
        Enemy -> [place_piece board newpos square]
        PassantEnemy -> [place_piece board newpos square]
        Empty -> place_piece board newpos square : (laser_beam board newpos square player_c laser)

bishop_func_list :: [SquarePos -> SquarePos]
bishop_func_list = [(\(x, y) -> (x + xd, y + yd)) | xd <- [1,-1], yd <- [1,-1]]

bishop_move :: Board -> SquarePos -> Square -> Color -> [Board]
bishop_move board pos square player_c = foldl (++) [] $ map (laser_beam board pos square player_c) bishop_func_list

rook_func_list :: [SquarePos -> SquarePos]
rook_func_list = [(\(x, y) -> (x + xd, y + yd)) | xd <- [0, 1,-1], yd <- [0, 1,-1], (==) 1 $ abs(xd) + abs(yd)]

rook_move :: Board -> SquarePos -> Square -> Color -> [Board]
rook_move board pos square player_c = foldl (++) [] $ map (laser_beam board pos square player_c) rook_func_list

queen_move :: Board -> SquarePos -> Square -> Color -> [Board]
queen_move a b c d = (rook_move a b c d) ++ (bishop_move a b c d)

king_move :: Board -> SquarePos -> Square -> Color -> [Board]
king_move board (row, col) square player_c = let
    _:poses = [(row + x, col + y) | x <- [0,1,-1], y <- [0,1,-1]]
    in foldl (++) [] $ map (piece_attempt_move board square player_c) poses

pos_move :: Board -> Square -> SquarePos -> Color -> [Board]
pos_move _ Nothing _ _ = []
pos_move board piece pos player_c = let
    board_minus_piece = place_piece board pos Nothing
    move_func = case pt of
        (Pawn _) -> pawn_move
        Knight -> knight_move
        Bishop -> bishop_move
        Rook -> rook_move
        Queen -> queen_move
        King -> king_move
    in if (piece_c /= player_c)
        then []
        else move_func board_minus_piece pos piece player_c
        where (Just (Piece pt piece_c)) = piece

possible_moves :: Board -> Color -> [Board]
possible_moves board player_c = let
    board_oriented = if (player_c == Black)
        then reverse board
        else board

    next_boards = foldl (++) [] [pos_move board_oriented ((board_oriented!!x)!!y) (x, y) player_c | x <- [0..7], y <- [0..7]]

    process_passant :: Square -> Square
    process_passant (Just (Piece (Pawn Passant) clr)) = Just $ Piece (Pawn Normal) clr
    process_passant (Just (Piece (Pawn MakeMePassant) clr)) = Just $ Piece (Pawn Passant) clr
    process_passant x = x

    passant_processed_board = map (map $ map process_passant) next_boards
    in if (player_c == Black)
        then map reverse passant_processed_board
        else passant_processed_board
