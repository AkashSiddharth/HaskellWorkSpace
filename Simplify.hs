{- Problem: Write a function 
                simplify :: Statement ->Statement
            that takes a Statement, stmt, and returns a Statement just like stmt, except that the 
            following simplifications are made:
            1. Each Statement of the form (IfStmt (VarExp "true") s) is replaced by a simplified 
               version of s.
            2. Each Expression of the form (BeginExp [] e) is replaced by a simplified version of e.
-}

module Simplify where
    import StatementsExpressions

    simplify :: Statement -> Statement

    simplify (IfStmt (VarExp "true") s) = (simplify s)
    simplify (IfStmt e s) = (IfStmt (simplifyExpression e) (simplify s))
    simplify (ExpStmt e) = (ExpStmt (simplifyExpression e))
    simplify (AssignStmt s e) = (AssignStmt s (simplifyExpression e))

    simplifyExpression :: Expression -> Expression

    simplifyExpression (BeginExp [] e) = (simplifyExpression e)
    simplifyExpression (BeginExp xs e) = (BeginExp [(simplify x) | x<-xs] (simplifyExpression e))
    simplifyExpression (EqualsExp e e2) = (EqualsExp (simplifyExpression e) (simplifyExpression e2))
    simplifyExpression (VarExp s) = (VarExp s)
    simplifyExpression (NumExp i) = (NumExp i)