structure BpTree =
struct
  datatype t = Br of int vector * t vector | Lf of string

  val empty = Lf ""

  val maxLength = 32
  val halfLength = maxLength div 2

  fun helpToString (Br (_, children), acc) =
        helpToStringBr (Vector.length children - 1, children, acc)
    | helpToString (Lf str, acc) = str :: acc

  and helpToStringBr (pos, children, acc) =
    if pos < 0 then
      acc
    else
      let
        val el = Vector.sub (children, pos)
        val acc = helpToString (el, acc)
      in
        helpToStringBr (pos - 1, children, acc)
      end

  fun toString rope =
    let val lst = helpToString (rope, [])
    in String.concat lst
    end

  datatype ins_result =
    Ok of int * t
  | S2 of int * t * int * t
  | S3 of int * t * int * t * int * t

  fun mkOk str =
    Ok (String.size str, Lf str)

  fun mkS2 (first, sec) =
    S2 (String.size first, Lf first, String.size sec, Lf sec)

  fun mkS3 (first, sec, third) =
    S3
      ( String.size first
      , Lf first
      , String.size sec
      , Lf sec
      , String.size third
      , Lf third
      )

  val targetLength = 1024

  fun isLessThanTarget (str1, str2) =
    String.size str1 + String.size str2 <= targetLength

  fun insLeaf (curIdx, oldStr, newStr) =
    if curIdx <= 0 then
      if isLessThanTarget (oldStr, newStr) then mkOk (newStr ^ oldStr)
      else mkS2 (newStr, oldStr)
    else if curIdx >= String.size oldStr then
      if isLessThanTarget (oldStr, newStr) then mkOk (oldStr ^ newStr)
      else mkS2 (oldStr, newStr)
    else
      (* Need to split in middle of string. *)
      let
        val sub1 = String.substring (oldStr, 0, curIdx)
        val sub2Len = String.size oldStr - curIdx
        val sub2 = String.substring (oldStr, curIdx, sub2Len)
      in
        if
          isLessThanTarget (oldStr, newStr)
        then
          mkOk (sub1 ^ newStr ^ sub2)
        else if
          curIdx + String.size newStr <= targetLength
        then
          mkS2 (sub1 ^ newStr, sub2)
        else if
          ((String.size oldStr) - curIdx) + String.size newStr <= targetLength
        then
          mkS2 (sub1, newStr ^ sub2)
        else
          mkS3 (sub1, newStr, sub2)
      end

  fun okBr (idxs, children, okLen, okChild) =
    let
      val lastEl = Vector.length idxs - 1
      val newIdx =
        Vector.mapi
          (fn (curIdx, curEl) => if curIdx < lastEl then curEl else okLen)
          idxs
      val totalIdx = Vector.foldl (op+) 0 newIdx

      val newChildren =
        Vector.mapi
          (fn (curIdx, curChild) =>
             if curIdx < lastEl then curChild else okChild) children
    in
      Ok (totalIdx, (Br (newIdx, newChildren)))
    end

  fun s2Fits (children, idxs, lidx, l, ridx, r) =
    let
      val newLen = Vector.length idxs + 1
      val newIdx = Vector.tabulate (newLen, fn idx =>
        if idx < newLen - 2 then Vector.sub (idxs, idx)
        else if idx = newLen - 2 then lidx
        else ridx)
      val totalIdx = Vector.foldl (op+) 0 newIdx

      val newChildren = Vector.tabulate (newLen, fn idx =>
        if idx < newLen - 2 then Vector.sub (children, idx)
        else if idx = newLen - 2 then l
        else r)
    in
      Ok (totalIdx, Br (newIdx, newChildren))
    end

  fun s2Split (idxs, children, lidx, l, ridx, r) =
    let
      val halfIdxs = Vector.length idxs div 2
      val leftIdxs = Vector.tabulate (halfIdxs, fn idx =>
        Vector.sub (idxs, idx))
      val leftTotal = Vector.foldl (op+) 0 leftIdxs

      val rightIdxsLen = Vector.length idxs - halfIdxs + 2
      val rightIdxs = Vector.tabulate (rightIdxsLen, fn idx =>
        if idx < rightIdxsLen - 1 then Vector.sub (idxs, idx + halfIdxs)
        else if idx = rightIdxsLen then lidx
        else ridx)
      val rightTotal = Vector.foldl (op+) 0 rightIdxs

      val halfChildren = Vector.length children div 2
      val leftChildren = Vector.tabulate (halfChildren, fn idx =>
        Vector.sub (children, idx))

      val rightChildrenLen = Vector.length children - halfChildren + 2
      val rightChildren = Vector.tabulate (rightIdxsLen, fn idx =>
        if idx < rightIdxsLen - 1 then Vector.sub (children, idx + halfChildren)
        else if idx = halfChildren then l
        else r)
    in
      S2
        ( leftTotal
        , Br (leftIdxs, leftChildren)
        , rightTotal
        , Br (rightIdxs, rightChildren)
        )
    end

  fun s3Fits (children, idxs, lidx, l, midx, m, ridx, r) =
    let
      val newIdx = Vector.tabulate (Vector.length idxs + 3, fn idx =>
        if idx < Vector.length idxs - 1 then Vector.sub (idxs, idx)
        else if idx = Vector.length idxs - 1 then lidx
        else if idx = Vector.length idxs then midx
        else ridx)
      val totalIdx = Vector.foldl (op+) 0 newIdx

      val newChildren = Vector.tabulate (Vector.length children + 3, fn idx =>
        if idx < Vector.length children - 1 then Vector.sub (children, idx)
        else if idx = Vector.length children - 1 then l
        else if idx = Vector.length idxs then m
        else r)
    in
      Ok (totalIdx, Br (newIdx, newChildren))
    end

  fun s3Split (idxs, children, lidx, l, midx, m, ridx, r) =
    let
      val halfIdxs = Vector.length idxs div 2
      val leftIdxs = Vector.tabulate (halfIdxs, fn idx =>
        Vector.sub (idxs, idx))
      val leftTotal = Vector.foldl (op+) 0 leftIdxs

      val rightIdxsLen = Vector.length idxs - halfIdxs + 3
      val rightIdxs = Vector.tabulate (rightIdxsLen, fn idx =>
        if idx < rightIdxsLen - 2 then Vector.sub (idxs, idx + halfIdxs)
        else if idx = rightIdxsLen - 2 then lidx
        else if idx = rightIdxsLen - 1 then midx
        else ridx)
      val rightTotal = Vector.foldl (op+) 0 rightIdxs

      val halfChildren = Vector.length children div 2
      val leftChildren = Vector.tabulate (halfChildren, fn idx =>
        Vector.sub (children, idx))

      val rightChildrenLen = Vector.length children - halfChildren + 3
      val rightChildren = Vector.tabulate (rightIdxsLen, fn idx =>
        if idx < rightIdxsLen - 2 then Vector.sub (children, idx + halfChildren)
        else if idx = rightChildrenLen - 2 then l
        else if idx = rightChildrenLen - 1 then m
        else r)
    in
      S2
        ( leftTotal
        , Br (leftIdxs, leftChildren)
        , rightTotal
        , Br (rightIdxs, rightChildren)
        )
    end

  fun app (newStr, Br (idxs, children)) =
        let
          val result = app (newStr, Vector.sub
            (children, Vector.length children - 1))
        in
          (case result of
             Ok (len, child) => okBr (idxs, children, len, child)
           | S2 (lidx, l, ridx, r) =>
               if Vector.length children + 2 <= maxLength then
                 s2Fits (children, idxs, lidx, l, ridx, r)
               else
                 s2Split (idxs, children, lidx, l, ridx, r)
           | S3 (lidx, l, midx, m, ridx, r) =>
               if Vector.length children + 3 <= maxLength then
                 s3Fits (children, idxs, lidx, l, midx, m, ridx, r)
               else
                 s3Split (idxs, children, lidx, l, midx, m, ridx, r))
        end
    | app (newStr, Lf oldStr) =
        insLeaf (String.size oldStr, oldStr, newStr)

  fun balRoot result =
    case result of
      Ok (_, t) => t
    | S2 (lidx, l, ridx, r) => Br (#[lidx, ridx], #[l, r])
    | S3 (lidx, l, midx, m, ridx, r) => Br (#[lidx, midx, ridx], #[l, m, r])

  fun append (newStr, tree) =
    let val result = app (newStr, tree)
    in balRoot result
    end

  fun helpLength (pos, idxs, acc) =
    if pos < 0 then
      acc
    else
      let
        val cur = Vector.sub (idxs, pos)
        val acc = acc + cur
      in
        helpLength (pos - 1, idxs, acc)
      end

  fun length (Br (idxs, _)) =
        helpLength (Vector.length idxs - 1, idxs, 0)
    | length (Lf str) = String.size str
end

fun timeFun (title, f) =
  let
    val title = String.concat ["Starting ", title, "..."]
    val _ = (print title)
    val startTime = Time.now ()
    val startTime = Time.toNanoseconds startTime
    val x = f ()
    val endTime = Time.now ()
    val endTime = Time.toNanoseconds endTime
    val timeDiff = endTime - startTime
    val timeDiff = LargeInt.toString timeDiff
    val timeTook = String.concat ["took ", timeDiff, " nanoseconds\n"]
    val _ = (print timeTook)
  in
    x
  end

fun appendMany (ctr, limit, rope) =
  if ctr = limit then
    rope
  else
    let val rope = BpTree.append ("hello, world!", rope)
    in appendMany (ctr + 1, limit, rope)
    end

val closure = fn () => (appendMany (0, 1000, BpTree.empty))
fun closure2 rope = fn () => BpTree.toString rope

fun main () =
  let
    val rope = timeFun ("b_tree append: ", closure)
    val str = timeFun ("b_tree toString: ", closure2 rope)
    val io = TextIO.openOut "hello.txt"
  in
    TextIO.output (io, str)
  end

val _ = main ()
