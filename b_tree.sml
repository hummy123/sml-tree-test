structure BTree = struct
  datatype 'a b_tree =
    Node of 'a vector * 'a b_tree vector

  val empty = Node (#[], #[])

  val maxLength = 32
  val halfLength = maxLength div 2

  datatype 'a ins_type =
    Ok of 'a b_tree
  | Max of 'a b_tree * 'a * 'a b_tree

  fun getLeftVals vals =
    Vector.tabulate (halfLength, fn idx => Vector.sub(vals, idx))

  fun getRightVals (vals, el) =
    let
      val rightValsLen = Vector.length vals - halfLength - 1
    in
      Vector.tabulate (rightValsLen, fn idx => 
        if idx < rightValsLen - 1 then
          Vector.sub(vals, halfLength + 1 + idx)
        else el)
    end

  fun getMedian vals =
    Vector.sub (vals, halfLength)

  fun appendLeaf (vlen: int, vals: 'a vector, children: 'a b_tree vector, el: 'a) =
    if vlen = 0 then
      Ok (Node (#[el], children))
    else if vlen < maxLength then
      let
        val vals = Vector.tabulate(vlen + 1, fn idx => if idx < vlen then
          Vector.sub(vals, idx) else el)
      in
        Ok (Node (vals, children))
      end
    else
      let
        val leftVals = getLeftVals vals
        val rightVals = getRightVals (vals, el)
        val left = Node (leftVals, #[])
        val right = Node (rightVals, #[])
        val median = getMedian vals
      in
        Max (left, median, right)
      end

  fun app (Node (vals, children), el) =
    let
      val vlen = Vector.length vals
      val clen = Vector.length children
    in
      if clen = 0 then 
        appendLeaf(vlen, vals, children, el)
      else
        let
          val result = app (Vector.sub(children, clen - 1), el)
        in
          case result of
            Ok last =>
              let
                val children = Vector.tabulate (clen, fn idx => 
                  if idx = clen - 1
                  then last 
                  else Vector.sub(children, idx))
              in
                Ok (Node (vals, children))
              end
          | Max (leftChild, median, rightChild) =>
              if clen + 2 <= maxLength then
                let
                  val vals = Vector.tabulate(vlen + 1, 
                    fn idx => 
                      if idx = vlen
                      then median
                      else Vector.sub (vals, idx))

                  val children = Vector.tabulate(clen + 2, 
                    fn idx =>
                      if idx = clen
                      then leftChild
                      else if idx = clen + 1
                      then rightChild
                      else Vector.sub (children, idx))

                  val node = Node (vals, children)
                in
                  Ok (node)
                end
             else 
               let
                 val leftChildren = getLeftVals children
                 val rightChildren = Vector.tabulate ((Vector.length children -
                   halfLength) + 2,
                    fn idx =>
                      if idx = Vector.length children - halfLength then
                        leftChild
                      else if idx = Vector.length children - halfLength + 1 then
                        rightChild
                      else
                        Vector.sub (children, idx + halfLength)
                      )

                val leftVals = getLeftVals vals
                val rightVals = getRightVals (vals, median)
                val newValMedian = getMedian vals

                val left = Node (leftVals, leftChildren)
                val right = Node (rightVals, rightChildren)
               in
                 Max (left, newValMedian, right)
               end
        end
    end

  fun append (tree, el) =
    case app (tree, el) of
      Ok t => t
    | Max (l, m, r) =>
        Node (#[m], #[l, r])

  fun insMany (tree) =
  let
    val tree = ref tree
    for ctr = 0 to 1000 do 
      tree := append (tree, ctr)
    done
  in
    tree
  end
    if ctr = 1000 then tree
    else
      let
        val tree = append (tree, ctr)
      in
        insMany (ctr + 1, tree)
      end

  val _ = insMany (0, empty)
end
