(*
 "/mnt/study/CSE/CoreofCore/ProgrammingLanguages/ProgrammingPartA/Module 4/HomeWork/HWS2.sml" 
 *)
(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string)=
    s1 = s2
fun all_except_option (s,ls)=
   let
      fun help(ls,acchead)=
       case ls of
         [] => NONE
         |head::tail=> if same_string(head,s) then SOME(acchead @ tail) else help(tail,head::acchead)
   in
      help(ls,[])
   end
fun get_substitutions1(lls,s)=
      case lls of
      []=>[]
      | [[]]=> []
      | (x::xs) :: rset=>case all_except_option(s,x::xs) of 
                     SOME(ls) => ls @ get_substitutions1(rset,s) 
                     | NONE=> get_substitutions1(rset,s) 
fun   get_substitutions2(lls,s)=
   let 
      fun help (lls,ans)=
         case lls of
         []=>ans
         | [[]]=> ans
         | (x::xs) :: rset=>case all_except_option(s,x::xs) of 
                        SOME(ls) => ls @ help(rset,ans) 
                        | NONE=> help(rset,ans) 
   in
      help(lls,[])
   end
fun   similar_names(lls,record_val)=
      case record_val of 
      {first = name1, middle = name2, last = name3} => let
                                                         fun   help(ls,acc)=
                                                            case ls of
                                                            []=> acc
                                                            | x::xs => help(xs,{first=x,middle=name2,last=name3}::acc)
                                                      in
                                                         {first=name1,middle=name2,last=name3}::help(get_substitutions2(lls,name1),[])
                                                      end
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
fun   card_color(Suit,Rank)=
   case Suit of 
   Clubs=> Black
   | Spades=>Black
   |_ => Red
fun card_value(Suit,Rank)=
   case Rank of 
   Num x=>x
   |_=>10

fun   remove_card(cs,c,e)=
   let
      fun help(ls,acchead)=
      case ls of
         [] => raise e
         |(s,r)::tail=> if (s,r) = c  then acchead @ tail  else help(tail,(s,r)::acchead)
   in
      help(cs,[])
   end
fun   all_same_color(lc)=
   case lc of
      []=>true
      |(s,r)::[]=>true
      |(s1,r1)::(s2,r2):: rest =>  
                                 let
                                    val x =card_color(s1,r1)
                                    val y = card_color(s2,r2)
                                 in 
                                    if x = y then all_same_color rest else false 
                                 end   
   fun   sum_cards(lc)=
      let 
         fun aux (ls,acc)=
            case ls of
               []=>acc
               |(s,Num x)::rest=>aux(rest,x+acc)
      in
         aux(lc,0)
      end
fun   score (lc,goal)=
   let
      val sum = sum_cards lc
      val same = all_same_color lc 
   in
     if same then 
        if sum > goal then (3 * (sum - goal)) div 2 
        else (goal - sum) div 2
     else 
        if sum > goal then (3 * (sum - goal)) div 2 
        else (goal - sum)
   end
   
fun   officiate(lc,lm,goal)=
      let 
         fun   aux(lc,lm,lh)=
            case lm of 
               []=>  score(lh,goal) 
               |(Discard c )::xs => aux(lc, xs, remove_card(lc, c, IllegalMove))
               |Draw::xs => case lc of 
                           []=> score(lh,goal)
                           |y::ys=> if sum_cards(y::lh) > goal 
                                   then score(y::lh,goal)
                                   else aux(ys,xs,y::lh)


      in
         aux(lc,lm,[])
      end

