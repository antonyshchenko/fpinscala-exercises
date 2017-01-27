package fpinscala

import org.scalatest._

class Chapter03Spec extends FunSpec with Matchers {
  describe("List") {
    describe("tail") {
      it("returns a list without first element") {
        List.tail(List(1, 2, 3)) shouldEqual List(2, 3)
      }

      it("throws exception when called on empty list") {
        an [RuntimeException] should be thrownBy {
          List.tail(List())
        }
      }
    }

    describe("setHead") {
      it("returns list with changed head") {
        List.setHead(List(1, 2, 3), 4) shouldEqual List(4, 2, 3)
      }

      it("throws exception when called on empty list") {
        an [RuntimeException] should be thrownBy {
          List.setHead(List(), 1)
        }
      }
    }

    describe("drop") {
      it("returns a list without first N elements") {
        List.drop(List(1, 2, 3), 2) shouldEqual List(3)
      }

      it("returns empty list when called on empty list") {
        List.drop(List(), 2) shouldEqual List()
      }

      it("returns same list when N is 0") {
        List.drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
      }

      it("returns same list when N is < 0") {
        List.drop(List(1, 2, 3), -1) shouldEqual List(1, 2, 3)
      }

      it("returns empty list when N is greater than the length of the original list") {
        List.drop(List(1, 2, 3), 5) shouldEqual List()
      }
    }

    describe("dropWhile") {
      val predicate = (x: Int) => x < 4

      it("returns a list without first elements that match predicate") {
        List.dropWhile(List(1, 2, 3, 4, 5), predicate) shouldEqual List(4, 5)
      }

      it("returns empty list when called on empty list") {
        List.dropWhile(List(), predicate) shouldEqual List()
      }

      it("returns same list when there are no elements that match predicate") {
        List.dropWhile(List(4, 5, 6), predicate) shouldEqual List(4, 5, 6)
      }
    }

    describe("init") {
      it("returns a list without last element") {
        List.init(List(1, 2, 3)) shouldEqual List(1, 2)
      }

      it("returns empty list when called on list with one element") {
        List.init(List(1)) shouldEqual List()
      }

      it("throws exception when called on empty list") {
        an [RuntimeException] should be thrownBy {
          List.init(List())
        }
      }
    }

    describe("length") {
      it("returns length of the list") {
        List.length(List(1, 2, 3)) shouldEqual 3
      }

      it("returns 0 when called on empty list") {
        List.length(List()) shouldEqual 0
      }
    }

    describe("foldLeft") {
      it("folds list to a single value") {
        List.foldLeft(List(1, 2, 3), 1)((x, y) => x + y) shouldEqual 7
      }

      it("returns seed when called on an empty list") {
        List.foldLeft(List[Int](), 1)((x, y) => x + y) shouldEqual 1
      }
    }

    describe("map") {
      it("returns list consisting of elements transformed with given function") {
        List.map(List(1, 2, 3))((x) => - x) shouldEqual List(-1, -2, -3)
      }

      it("returns empty list when called on empty list") {
        List.map(List[Int]())((x) => x) shouldEqual List[Int]()
      }
    }

    describe("sum") {
      it("returns a sum of the list elements") {
        List.sum(List(1, 2, 3)) shouldEqual 6
      }

      it("returns 0 when called on empty list") {
        List.sum(List[Int]()) shouldEqual 0
      }
    }

    describe("product") {
      it("returns a product of the list elements") {
        List.product(List(1, 2, 3, 4)) shouldEqual 24.0
      }

      it("returns 1 when called on empty list") {
        List.product(List[Double]()) shouldEqual 1.0
      }

      it("returns 0 when list elements contain 0") {
        List.product(List(0.0, 2)) shouldEqual 0.0
      }
    }

    describe("lengthOptimized") {
      it("returns length of the list") {
        List.lengthOptimized(List(1, 2, 3)) shouldEqual 3
      }

      it("returns 0 when called on empty list") {
        List.lengthOptimized(List()) shouldEqual 0
      }
    }

    describe("reverse") {
      it("returns a reversed list") {
        List.reverse(List(1, 2, 3)) shouldEqual List(3, 2, 1)
      }

      it("returns empty list when called on empty list") {
        List.reverse(List[Int]()) shouldEqual List[Int]()
      }
    }

    describe("foldRightOptimized") {
      it("folds list to a single value") {
        List.foldRightOptimized(List(1, 2, 3), 1)((x, y) => x + y) shouldEqual 7
      }

      it("returns seed when called on an empty list") {
        List.foldRightOptimized(List[Int](), 1)((x, y) => x + y) shouldEqual 1
      }
    }

    describe("append") {
      it("returns a list which is a result of joining two lists") {
        val list1 = List(1, 2, 3)
        val list2 = List(4, 5, 6)

        List.append(list1, list2) shouldEqual List(1, 2, 3, 4, 5, 6)
      }

      it("returns first argument when second argument is an empty list") {
        val list1 = List(1, 2, 3)
        val list2 = List[Int]()

        List.append(list1, list2) shouldEqual List(1, 2, 3)
      }

      it("returns second argument when first argument is an empty list") {
        val list1 = List[Int]()
        val list2 = List(4, 5, 6)

        List.append(list1, list2) shouldEqual List(4, 5, 6)
      }

      it("returns empty list when both arguments are empty lists") {
        val list1 = List[Int]()
        val list2 = List[Int]()

        List.append(list1, list2) shouldEqual List[Int]()
      }
    }

    describe("incrementByOne") {
      it("returns a list where each element of original list is incremented by one") {
        List.incrementByOne(List(1, 2, 3)) shouldEqual List(2, 3, 4)
      }

      it("returns an empty list when called on empty lists") {
        List.incrementByOne(List[Int]()) shouldEqual List[Int]()
      }
    }

    describe("toListOfStrings") {
      it("returns a list where each element of original list is represented as string") {
        List.toListOfStrings(List(1, 2, 3)) shouldEqual List("1.0", "2.0", "3.0")
      }

      it("returns an empty list when called on empty lists") {
        List.toListOfStrings(List[Double]()) shouldEqual List[String]()
      }
    }

    describe("filter") {
      val predicate = (x:Int) => x % 2 == 0

      it("returns a list with elements of original list for which a given predicate returns true") {
        List.filter(List(1, 2, 3, 4, 5, 6))(predicate) shouldEqual List(2, 4, 6)
      }

      it("returns an empty list when called on empty list") {
        List.filter(List[Int]())(predicate) shouldEqual List[Int]()
      }

      it("returns an empty list when original list does not contain any element for which predicate returns true") {
        List.filter(List(1, 3, 5))(predicate) shouldEqual List[Int]()
      }
    }

    describe("flatten") {
      it("returns a flattened list") {
        List.flatten(List(List(1, 2), List(3, 4))) shouldEqual List(1, 2, 3, 4)
      }

      it("returns empty list when called on empty list") {
        List.flatten(List[List[Int]]()) shouldEqual List[Int]()
      }

      it("returns empty list when called on a list of empty lists") {
        List.flatten(List(List[Int](), List[Int]())) shouldEqual List[Int]()
      }
    }

    describe("flatMap") {
      it("returns a flattened list of elements as a result of mapping over given list with a function that returns a list for each element") {
        List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
      }

      it("returns an empty list when called on empty list") {
        List.flatMap(List[Int]())(i => List(i, i)) shouldEqual List[Int]()
      }
    }

    describe("filterThroughFlatmap") {
      val predicate = (x:Int) => x % 2 == 0

      it("returns a list with elements of original list for which a given predicate returns true") {
        List.filterThroughFlatmap(List(1, 2, 3, 4, 5, 6))(predicate) shouldEqual List(2, 4, 6)
      }

      it("returns an empty list when called on empty list") {
        List.filterThroughFlatmap(List[Int]())(predicate) shouldEqual List[Int]()
      }

      it("returns an empty list when original list does not contain any element for which predicate returns true") {
        List.filterThroughFlatmap(List(1, 3, 5))(predicate) shouldEqual List[Int]()
      }
    }

    describe("zipWithSum") {
      it("returns a list where element on position i is a sum of elements at the same position in lists given as arguments") {
        val l1 = List(1, 2, 3)
        val l2 = List(4, 5, 6)

        List.zipWithSum(l1, l2) shouldEqual List(5, 7, 9)
      }

      it("returns an empty list when called on empty lists") {
        List.zipWithSum(List[Int](), List[Int]()) shouldEqual List[Int]()
      }

      it("returns list which is as long as shortest of the argument lists") {
        val l1 = List(1, 2)
        val l2 = List(4, 5, 6)

        List.zipWithSum(l1, l2) shouldEqual List(5, 7)
        List.zipWithSum(l2, l1) shouldEqual List(5, 7)
      }
    }

    describe("zipWith") {
      val f = (x: Int, y: Int) => x + y

      it("returns a list where element on position i is a result of applying a function f to elements at the same position in lists given as arguments") {
        val l1 = List(1, 2, 3)
        val l2 = List(4, 5, 6)

        List.zipWith(l1, l2)(f) shouldEqual List(5, 7, 9)
      }

      it("returns an empty list when called on empty lists") {
        List.zipWith(List[Int](), List[Int]())(f) shouldEqual List[Int]()
      }

      it("returns list which is as long as shortest of the argument lists") {
        val l1 = List(1, 2)
        val l2 = List(4, 5, 6)

        List.zipWith(l1, l2)(f) shouldEqual List(5, 7)
        List.zipWith(l2, l1)(f) shouldEqual List(5, 7)
      }
    }

    describe("hasSubsequence") {
      it("returns true when first argument contains second argument") {
        val sequence = List(1, 2, 3, 4)
        val subsequence = List(2, 3)

        List.hasSubsequence(sequence, subsequence) shouldEqual true
      }

      it("returs true when called on empty lists") {
        List.hasSubsequence(List[Int](), List[Int]()) shouldEqual true
      }

      it("returns false when first argument does not contain second argument") {
        val sequence = List(1, 2, 3, 4)
        val subsequence = List(7, 8)

        List.hasSubsequence(sequence, subsequence) shouldEqual false
      }

      it("returns false when subsequence is longer than sequence") {
        val sequence = List(1, 2)
        val subsequence = List(1, 2, 3)

        List.hasSubsequence(sequence, subsequence) shouldEqual false
      }

      it("returns false when sequence is empty") {
        val sequence = List[Int]()
        val subsequence = List(1, 2, 3)

        List.hasSubsequence(sequence, subsequence) shouldEqual false
      }

      it("returns true when sequence is not empty, but subsequence is empty") {
        val sequence = List(1, 2, 3)
        val subsequence = List[Int]()

        List.hasSubsequence(sequence, subsequence) shouldEqual true
      }
    }
  }

  describe("Tree") {
    val tree = Branch(
      left = Branch(
        left = Leaf(1),
        right = Leaf(2)
      ),
      right = Leaf(-3)
    )

    val invertedTree = Branch(
      left = Branch(
        left = Leaf(-1),
        right = Leaf(-2)
      ),
      right = Leaf(3)
    )

    describe("size") {
      it("returns number of nodes (leaves and branches) in the tree") {
        Tree.size(tree) shouldEqual 5
      }
    }

    describe("maximum") {
      it("returns maximum element in the tree") {
        Tree.maximum(tree) shouldEqual 2
      }

      it("returns the value of a single node tree") {
        Tree.maximum(Leaf(123)) shouldEqual 123
      }
    }

    describe("depth") {
      it("returns maximum path length from a root of the tree to any of the leafs") {
        Tree.depth(tree) shouldEqual 2
      }

      it("returns 0 for single node tree") {
        Tree.depth(Leaf(123)) shouldEqual 0
      }
    }

    describe("map") {
      it("returns a copy of a given tree where each leaf value is transformed with given function") {
        Tree.map(tree)(x => -x) shouldEqual(invertedTree)
      }
    }

    describe("fold") {
      it("can be used to implement size function") {
        def sizeUsingFold[A](tree: Tree[A]): Int = {
          Tree.fold(tree)(_ => 1)((l, r) => 1 + l + r)
        }

        sizeUsingFold(tree) shouldEqual 5
      }

      it("can be used to implement maximum function") {
        def maximumUsingFold(tree: Tree[Int]): Int = {
          Tree.fold(tree)(x => x)((l, r) => l.max(r))
        }

        maximumUsingFold(tree) shouldEqual 2
      }

      it("can be used to implement depth function") {
        def depthUsingFold[A](tree: Tree[A]): Int = {
          Tree.fold(tree)(_ => 0)((l, r) => 1 + l + r)
        }

        depthUsingFold(tree) shouldEqual 2
      }

      it("can be used to implement map function") {
        def mapUsingFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
          Tree.fold(tree) { x => Leaf(f(x)): Tree[B] } { (l, r) => Branch(l, r) }
        }

        mapUsingFold(tree)(x => -x) shouldEqual invertedTree
      }
    }
  }
}

