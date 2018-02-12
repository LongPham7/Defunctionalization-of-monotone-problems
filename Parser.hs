{-# OPTIONS_GHC -w #-}
module Parser where
import DataTypes
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 ([(String, Sort)])
	| HappyAbsSyn6 ((String, Sort))
	| HappyAbsSyn7 (Sort)
	| HappyAbsSyn8 ([(String, Term)])
	| HappyAbsSyn9 ((String, Term))
	| HappyAbsSyn10 (Term)

action_0 (15) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (15) = happyShift action_2
action_1 _ = happyFail

action_2 (5) = happyGoto action_4
action_2 _ = happyReduce_2

action_3 (40) = happyAccept
action_3 _ = happyFail

action_4 (16) = happyShift action_6
action_4 (23) = happyShift action_7
action_4 (6) = happyGoto action_5
action_4 _ = happyFail

action_5 _ = happyReduce_3

action_6 (17) = happyShift action_9
action_6 _ = happyFail

action_7 (8) = happyGoto action_8
action_7 _ = happyReduce_9

action_8 (16) = happyShift action_15
action_8 (39) = happyShift action_16
action_8 (9) = happyGoto action_14
action_8 _ = happyFail

action_9 (18) = happyShift action_11
action_9 (19) = happyShift action_12
action_9 (21) = happyShift action_13
action_9 (7) = happyGoto action_10
action_9 _ = happyFail

action_10 (20) = happyShift action_30
action_10 _ = happyReduce_4

action_11 _ = happyReduce_5

action_12 _ = happyReduce_6

action_13 (18) = happyShift action_11
action_13 (19) = happyShift action_12
action_13 (21) = happyShift action_13
action_13 (7) = happyGoto action_29
action_13 _ = happyFail

action_14 (26) = happyShift action_28
action_14 _ = happyFail

action_15 (25) = happyShift action_27
action_15 _ = happyFail

action_16 (16) = happyShift action_22
action_16 (21) = happyShift action_23
action_16 (24) = happyShift action_24
action_16 (27) = happyShift action_25
action_16 (38) = happyShift action_26
action_16 (10) = happyGoto action_17
action_16 (11) = happyGoto action_18
action_16 (12) = happyGoto action_19
action_16 (13) = happyGoto action_20
action_16 (14) = happyGoto action_21
action_16 _ = happyFail

action_17 (16) = happyShift action_22
action_17 (21) = happyShift action_23
action_17 (24) = happyShift action_24
action_17 (27) = happyShift action_25
action_17 (29) = happyShift action_48
action_17 (30) = happyShift action_49
action_17 (38) = happyShift action_26
action_17 (10) = happyGoto action_47
action_17 (11) = happyGoto action_18
action_17 (12) = happyGoto action_19
action_17 (13) = happyGoto action_20
action_17 _ = happyReduce_32

action_18 _ = happyReduce_14

action_19 _ = happyReduce_13

action_20 (31) = happyShift action_40
action_20 (32) = happyShift action_41
action_20 (33) = happyShift action_42
action_20 (34) = happyShift action_43
action_20 (35) = happyShift action_44
action_20 (36) = happyShift action_45
action_20 (37) = happyShift action_46
action_20 _ = happyReduce_12

action_21 _ = happyReduce_1

action_22 _ = happyReduce_27

action_23 (16) = happyShift action_22
action_23 (21) = happyShift action_23
action_23 (24) = happyShift action_24
action_23 (27) = happyShift action_25
action_23 (38) = happyShift action_26
action_23 (10) = happyGoto action_36
action_23 (11) = happyGoto action_37
action_23 (12) = happyGoto action_38
action_23 (13) = happyGoto action_39
action_23 _ = happyFail

action_24 (16) = happyShift action_35
action_24 _ = happyFail

action_25 (16) = happyShift action_34
action_25 _ = happyFail

action_26 _ = happyReduce_28

action_27 (16) = happyShift action_22
action_27 (21) = happyShift action_23
action_27 (24) = happyShift action_24
action_27 (27) = happyShift action_25
action_27 (38) = happyShift action_26
action_27 (10) = happyGoto action_33
action_27 (11) = happyGoto action_18
action_27 (12) = happyGoto action_19
action_27 (13) = happyGoto action_20
action_27 _ = happyFail

action_28 _ = happyReduce_10

action_29 (20) = happyShift action_30
action_29 (22) = happyShift action_32
action_29 _ = happyFail

action_30 (18) = happyShift action_11
action_30 (19) = happyShift action_12
action_30 (21) = happyShift action_13
action_30 (7) = happyGoto action_31
action_30 _ = happyFail

action_31 (20) = happyShift action_30
action_31 _ = happyReduce_8

action_32 _ = happyReduce_7

action_33 (16) = happyShift action_22
action_33 (21) = happyShift action_23
action_33 (24) = happyShift action_24
action_33 (27) = happyShift action_25
action_33 (29) = happyShift action_48
action_33 (30) = happyShift action_49
action_33 (38) = happyShift action_26
action_33 (10) = happyGoto action_47
action_33 (11) = happyGoto action_18
action_33 (12) = happyGoto action_19
action_33 (13) = happyGoto action_20
action_33 _ = happyReduce_11

action_34 (17) = happyShift action_64
action_34 _ = happyFail

action_35 (17) = happyShift action_63
action_35 _ = happyFail

action_36 (16) = happyShift action_22
action_36 (21) = happyShift action_23
action_36 (24) = happyShift action_24
action_36 (27) = happyShift action_25
action_36 (29) = happyShift action_48
action_36 (30) = happyShift action_49
action_36 (38) = happyShift action_26
action_36 (10) = happyGoto action_47
action_36 (11) = happyGoto action_18
action_36 (12) = happyGoto action_19
action_36 (13) = happyGoto action_20
action_36 _ = happyFail

action_37 (22) = happyShift action_62
action_37 _ = happyReduce_14

action_38 (22) = happyShift action_61
action_38 _ = happyReduce_13

action_39 (22) = happyShift action_60
action_39 (31) = happyShift action_40
action_39 (32) = happyShift action_41
action_39 (33) = happyShift action_42
action_39 (34) = happyShift action_43
action_39 (35) = happyShift action_44
action_39 (36) = happyShift action_45
action_39 (37) = happyShift action_46
action_39 _ = happyReduce_12

action_40 (16) = happyShift action_22
action_40 (21) = happyShift action_53
action_40 (38) = happyShift action_26
action_40 (13) = happyGoto action_59
action_40 _ = happyFail

action_41 (16) = happyShift action_22
action_41 (21) = happyShift action_53
action_41 (38) = happyShift action_26
action_41 (13) = happyGoto action_58
action_41 _ = happyFail

action_42 (16) = happyShift action_22
action_42 (21) = happyShift action_53
action_42 (38) = happyShift action_26
action_42 (13) = happyGoto action_57
action_42 _ = happyFail

action_43 (16) = happyShift action_22
action_43 (21) = happyShift action_53
action_43 (38) = happyShift action_26
action_43 (13) = happyGoto action_56
action_43 _ = happyFail

action_44 (16) = happyShift action_22
action_44 (21) = happyShift action_53
action_44 (38) = happyShift action_26
action_44 (13) = happyGoto action_55
action_44 _ = happyFail

action_45 (16) = happyShift action_22
action_45 (21) = happyShift action_53
action_45 (38) = happyShift action_26
action_45 (13) = happyGoto action_54
action_45 _ = happyFail

action_46 (16) = happyShift action_22
action_46 (21) = happyShift action_53
action_46 (38) = happyShift action_26
action_46 (13) = happyGoto action_52
action_46 _ = happyFail

action_47 (16) = happyShift action_22
action_47 (21) = happyShift action_23
action_47 (24) = happyShift action_24
action_47 (27) = happyShift action_25
action_47 (29) = happyShift action_48
action_47 (30) = happyShift action_49
action_47 (38) = happyShift action_26
action_47 (10) = happyGoto action_47
action_47 (11) = happyGoto action_18
action_47 (12) = happyGoto action_19
action_47 (13) = happyGoto action_20
action_47 _ = happyReduce_20

action_48 (16) = happyShift action_22
action_48 (21) = happyShift action_23
action_48 (24) = happyShift action_24
action_48 (27) = happyShift action_25
action_48 (38) = happyShift action_26
action_48 (10) = happyGoto action_51
action_48 (11) = happyGoto action_18
action_48 (12) = happyGoto action_19
action_48 (13) = happyGoto action_20
action_48 _ = happyFail

action_49 (16) = happyShift action_22
action_49 (21) = happyShift action_23
action_49 (24) = happyShift action_24
action_49 (27) = happyShift action_25
action_49 (38) = happyShift action_26
action_49 (10) = happyGoto action_50
action_49 (11) = happyGoto action_18
action_49 (12) = happyGoto action_19
action_49 (13) = happyGoto action_20
action_49 _ = happyFail

action_50 (16) = happyShift action_22
action_50 (21) = happyShift action_23
action_50 (24) = happyShift action_24
action_50 (27) = happyShift action_25
action_50 (38) = happyShift action_26
action_50 (10) = happyGoto action_47
action_50 (11) = happyGoto action_18
action_50 (12) = happyGoto action_19
action_50 (13) = happyGoto action_20
action_50 _ = happyReduce_17

action_51 (16) = happyShift action_22
action_51 (21) = happyShift action_23
action_51 (24) = happyShift action_24
action_51 (27) = happyShift action_25
action_51 (30) = happyShift action_49
action_51 (38) = happyShift action_26
action_51 (10) = happyGoto action_47
action_51 (11) = happyGoto action_18
action_51 (12) = happyGoto action_19
action_51 (13) = happyGoto action_20
action_51 _ = happyReduce_16

action_52 (31) = happyShift action_40
action_52 (32) = happyShift action_41
action_52 _ = happyReduce_24

action_53 (16) = happyShift action_22
action_53 (21) = happyShift action_53
action_53 (38) = happyShift action_26
action_53 (13) = happyGoto action_67
action_53 _ = happyFail

action_54 (31) = happyShift action_40
action_54 (32) = happyShift action_41
action_54 _ = happyReduce_25

action_55 (31) = happyShift action_40
action_55 (32) = happyShift action_41
action_55 _ = happyReduce_23

action_56 (31) = happyShift action_40
action_56 (32) = happyShift action_41
action_56 _ = happyReduce_22

action_57 (31) = happyShift action_40
action_57 (32) = happyShift action_41
action_57 _ = happyReduce_21

action_58 _ = happyReduce_30

action_59 _ = happyReduce_29

action_60 _ = happyReduce_31

action_61 _ = happyReduce_26

action_62 _ = happyReduce_15

action_63 (18) = happyShift action_11
action_63 (19) = happyShift action_12
action_63 (21) = happyShift action_13
action_63 (7) = happyGoto action_66
action_63 _ = happyFail

action_64 (18) = happyShift action_11
action_64 (19) = happyShift action_12
action_64 (21) = happyShift action_13
action_64 (7) = happyGoto action_65
action_64 _ = happyFail

action_65 (20) = happyShift action_30
action_65 (28) = happyShift action_69
action_65 _ = happyFail

action_66 (20) = happyShift action_30
action_66 (28) = happyShift action_68
action_66 _ = happyFail

action_67 (22) = happyShift action_60
action_67 (31) = happyShift action_40
action_67 (32) = happyShift action_41
action_67 _ = happyFail

action_68 (21) = happyShift action_71
action_68 _ = happyFail

action_69 (21) = happyShift action_70
action_69 _ = happyFail

action_70 (16) = happyShift action_22
action_70 (21) = happyShift action_23
action_70 (24) = happyShift action_24
action_70 (27) = happyShift action_25
action_70 (38) = happyShift action_26
action_70 (10) = happyGoto action_73
action_70 (11) = happyGoto action_18
action_70 (12) = happyGoto action_19
action_70 (13) = happyGoto action_20
action_70 _ = happyFail

action_71 (16) = happyShift action_22
action_71 (21) = happyShift action_23
action_71 (24) = happyShift action_24
action_71 (27) = happyShift action_25
action_71 (38) = happyShift action_26
action_71 (10) = happyGoto action_72
action_71 (11) = happyGoto action_18
action_71 (12) = happyGoto action_19
action_71 (13) = happyGoto action_20
action_71 _ = happyFail

action_72 (16) = happyShift action_22
action_72 (21) = happyShift action_23
action_72 (22) = happyShift action_75
action_72 (24) = happyShift action_24
action_72 (27) = happyShift action_25
action_72 (29) = happyShift action_48
action_72 (30) = happyShift action_49
action_72 (38) = happyShift action_26
action_72 (10) = happyGoto action_47
action_72 (11) = happyGoto action_18
action_72 (12) = happyGoto action_19
action_72 (13) = happyGoto action_20
action_72 _ = happyFail

action_73 (16) = happyShift action_22
action_73 (21) = happyShift action_23
action_73 (22) = happyShift action_74
action_73 (24) = happyShift action_24
action_73 (27) = happyShift action_25
action_73 (29) = happyShift action_48
action_73 (30) = happyShift action_49
action_73 (38) = happyShift action_26
action_73 (10) = happyGoto action_47
action_73 (11) = happyGoto action_18
action_73 (12) = happyGoto action_19
action_73 (13) = happyGoto action_20
action_73 _ = happyFail

action_74 _ = happyReduce_18

action_75 _ = happyReduce_19

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (MProblem happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2:happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 ((happy_var_1, happy_var_3)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn7
		 (IntSort
	)

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (BoolSort
	)

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  8 happyReduction_9
happyReduction_9  =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_2:happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1,happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (And happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Or happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 8 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Lambda happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 8 11 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Exists happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  11 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Sma happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (SmaEq happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  12 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Lar happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (LarEq happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  13 happyReduction_27
happyReduction_27 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 (Var happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  13 happyReduction_28
happyReduction_28 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn10
		 (Num happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Add happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  14 happyReduction_32
happyReduction_32 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenEnvDec -> cont 15;
	TokenVar happy_dollar_dollar -> cont 16;
	TokenColon -> cont 17;
	TokenIntSort -> cont 18;
	TokenBoolSort -> cont 19;
	TokenArrow -> cont 20;
	TokenLeftParen -> cont 21;
	TokenRightParen -> cont 22;
	TokenProgramDec -> cont 23;
	TokenExists -> cont 24;
	TokenDef -> cont 25;
	TokenEndMarker -> cont 26;
	TokenLambda -> cont 27;
	TokenPeriod -> cont 28;
	TokenAnd -> cont 29;
	TokenOr -> cont 30;
	TokenAdd -> cont 31;
	TokenSub -> cont 32;
	TokenSmaller -> cont 33;
	TokenSmallerEq -> cont 34;
	TokenEq -> cont 35;
	TokenLarger -> cont 36;
	TokenLargerEq -> cont 37;
	TokenNum happy_dollar_dollar -> cont 38;
	TokenGoalDec -> cont 39;
	_ -> happyError' (tk:tks)
	}

happyError_ 40 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError s = error (show s)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc8440_0\\ghc_2.h" #-}
















































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
