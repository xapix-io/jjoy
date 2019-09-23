(ns jjoy.lib)

(def lib
  '{keep (over [call] dip)
    if (? call)
    when (swap [ call ] [ drop ] if)
    dip2 (swap [dip] dip)
    do (dup dip2)
    curry (swap prefix)
    while (swap do concat [ loop ] curry when)

    rest (dup length dup 0 >
              [1 swap subvec]
              [drop drop []]
              if)

    loop ([ call ] keep [ loop ] curry when)

    map ([]
         [overd swap length 0 >]
         [overd swap 0 nth overd swap call ] while)
    seq.prefix (over [ over length 1 + ] dip
                     [ (sequence) [ 1 swap copy-unsafe ] keep ] new-like)})

(defn map2
  ([l f] (map2 [] l f))
  ([acc [x & l] f]
   (if x
     (map2 (conj acc (f x)) l f)
     acc)))

(comment
  ;; MAP IMPL
  l-old f l-new
  []
  ["abc-abcaa" shuffle length 0 >]
  [0 nth
   "abc-abca" shuffle call
   suffix] while
  "abcd-c" shuffle
  ;; WHILE IMPL
  swap call when

  ;; FACT IMPL
  dup [ dup 1 > ] [ 1 - dup [ * ] dip ] while drop

  5 [dup prn 1 - 0 >] loop 


  l1 l2
  [dup length 0 >]
  [0 nth]
  while
  drop


  l1 l2 dup 0 nth
  l1 l2 v vec swap 

  ...

  l1 l2 [v suffix] dip
  )

(comment
  l-old f l-new overd
  l-old f l-old l-new swap

  ...
  l-old f l-new l-old


  l-old f l-new b

  l-old f l-new overd swap
  l-old f l-new l-old 0 nth
  l-old f l-new v overd swap

  ...
  l-old f l-new v f call

  l-old f l-new v' vec swap prefix
  l-old f l-new [v'] [prefix] concat

  ...
  l-old f l-new [v' prefix] dip2
  )
