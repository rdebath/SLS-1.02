/^  yyInfinite = [0-9][0-9]*;$/s/[0-9][0-9]*/65535/
/^  yyInfoType =$/,/^    END;$/{
/Cost: INTEGER;/{
h
s/INTEGER; Proc: yy[A-Za-z0-9]*/SHORTCARD/w /tmp/yyCostType
g
s/Cost: INTEGER; //
}
/^    END;$/{
a\
\
\ \ yyCostType =\
\ \ \ \ RECORD\
\ \ \ \ \ \ (* $1 *)\
\ \ \ \ END;
}
}
/^  yyInfo: yyInfoType;$/a\
\ \ yyCostInfo : yyCostType;
/^PROCEDURE Cost[A-Za-z0-9]* (yyt: Tree.tTree): INTEGER;$/,/^END Cost[A-Za-z0-9]*;$/d
/^  cost: INTEGER;$/s/INTEGER/SHORTCARD/
/^  info: yyInfoPtr;$/a\
\ \ CostInfo: yyCostType;
/^  yyt\^\.yyEstraInfo := info;$/a\
\ \ CostInfo := yyCostInfo;
1,$s/IF cost < info\^\.\([A-Za-z][A-Za-z0-9]*\)\.Cost THEN/IF cost < CostInfo.\1.Cost THEN/g
1,$s/info\^\.\([A-Za-z][A-Za-z0-9]*\)\.Cost := cost;/CostInfo.\1.Cost := cost;/g
/^  WITH yyInfo DO$/,/^  END;$/{
/^    [A-Za-z][A-Za-z0-9]*\.Cost := yyInfinite;/{
w /tmp/yyCostInfo
d
}
/^  END;$/{
a\
\ \ WITH yyCostInfo DO\
\ \ \ \ (* $2 *)\
\ \ END;
}
}
/^  IF LONGINT (yyPoolEndPtr - yyPoolFreePtr) < SYSTEM.TSIZE (yyInfoType) THEN$/a\
\ \ \ \ INC (HeapUsed, yyPoolSize);
