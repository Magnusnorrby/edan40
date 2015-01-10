module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T | Skip | Begin [Statement] | If Expr.T Statement Statement | While Expr.T Statement | Read String | Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> Begin

ifStmt = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((e, s1), s2)= If e s1 s2

whileStmt = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
buildWhile (e, s) = While e s 

readStmt = accept "read" -# word #- require ";" >-> Read


write = accept "write" -# Expr.parse #- require ";" >-> Write




exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (Assignment v e: stmts) dict input = exec stmts newDict input
     where newDict = Dictionary.insert (v, Expr.value e dict) dict

exec (Skip: stmts) dict input = exec stmts dict input

exec (Begin sl: stmts) dict input = exec newStmts dict input
     where newStmts = sl ++ stmts

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While e s: stmts) dict input  
   | eval > 0 =  exec (s:whileStmt:stmts) dict input
   | otherwise = exec stmts dict input
   where eval = Expr.value e dict
   	 whileStmt = While e s

exec (Read s: stmts) dict (i:is) = exec stmts newDict is
     where newDict = Dictionary.insert (s, i) dict

exec (Write e: stmts) dict input = eval : exec stmts dict input
     where eval = Expr.value e dict

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStmt ! whileStmt ! readStmt ! write

  toString (Assignment v e) = v ++ " := " ++ Expr.toString e ++ ";\n"
  toString (Skip) = "skip;\n"
  toString (Begin sl) = "begin\n" ++ printSl ++ "end\n"
  	   where printSl = foldr1 (++) $ map toString sl
  toString (If cond thenStmts elseStmts) = "if " ++ Expr.toString cond ++ " then\n" ++ Expr.toString thenStmts ++ "else\n" ++ Expr.toString elseStmts 
  toString (While e s) = "while " ++ Expr.toString e ++ " do\n" ++ Expr.toString s
  toString (Read s) = "read " ++ s ++";\n"
  toString (Write e) = "write " ++ Expr.toString e ++ ";\n"
