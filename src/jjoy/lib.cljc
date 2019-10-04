(ns jjoy.lib)

(def lib
  '{keep (over [call] dip)
    if (? call)
    panic? ([panic] =)
    when (swap [ call ] [ drop ] if)
    ;; FIXME rename dip2 -> 2dip
    dip2 (swap [dip] dip)
    do (dup dip2)
    curry (swap prefix)
    while (swap do concat [ loop ] curry when)

    dup ("a-aa" shuffle)
    swap ("ab-ba" shuffle)
    drop ("a-" shuffle)
    over ("ab-aba" shuffle)
    rot ("abc-bca" shuffle)

    rest ("[a..b]-[..b]" shuffle)
    prefix ("ab-[b..a]" shuffle)
    suffix ("ab-[..ab]" shuffle)

    first (0 nth)

    loop ([ call ] keep [ loop ] curry when)

    each (["ab-aba" shuffle length 0 >]
          ["[x..a]b-xabb" shuffle dip2]
          while
          "ab-" shuffle)

    map ([swap] [dip "ab-[..ba]" shuffle] "abc-[..ba..c]" shuffle
         []
         "abc-cab" shuffle
         each)

    join ("a-[a]" shuffle consume drop
          dup panic? [panic!] when)

    pmap (["a-[a]" shuffle] [spawn] "abc-[..ba..c]" shuffle map
          [join first] map)})

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
  ["abc-abca" shuffle length 0 >]
  ["[x..a]bc-abcxb" shuffle call
   "abcx-ab[..cx]" shuffle]
  while
  "abc-c" shuffle
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
