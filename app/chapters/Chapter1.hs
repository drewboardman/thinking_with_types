module Chapter1 where

data Three = One | Two | Three

newtype TicTacToe a = TicTacToe { board :: Three -> Three -> a }
