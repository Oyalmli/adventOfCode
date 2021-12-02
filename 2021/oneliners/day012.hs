import Control.Monad;main=interact$show.sum.(zipWith((fromEnum.).(<))<*>tail).(ap(zipWith3(((+).).(+)))tail`ap`drop 2).map read.words
