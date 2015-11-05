#!/usr/bin/python


class Node:
    def __init__(self, value, left, right):
        self.value = value
        self.left = left
        self.right = right

    def __str__(self):
        return "Node {}".format(self.value)

tree0 = (
    Node(7,
        Node(5,
            Node(3, None, None),
            Node(8, None, None),
        ),
        Node(1,
            Node(0, None, None),
            Node(6, None, None),
        ),
    )
)


tree1 = (
         Node("a",
              Node("b",
                   Node("d",
                        Node("g", None, None),
                        Node("h", None, None)),
                   Node("e",
                        None,
                        Node("i", None, None)
                        )
                   ),
              Node("c",
                   Node("f",
                        None,
                        Node("j", None, None)
                        ),
                   None)
              )
         )


tree2 = (
         Node("/",
              Node("*",
                   Node("+",
                        Node("a", None, None),
                        Node("b", None, None)
                        ),
                   Node("-",
                        Node("c", None, None),
                        Node("d", None, None)
                        )
                   ),
              Node("+",
                   Node("e", None, None),
                   Node("f", None, None)
                   ),
              )
         )


def inorder_non_rec(tree):
    stack_going = []
    stack_returning = []
    stack_going.append(tree)

    while stack_going or stack_returning:
        if stack_going:
            node = stack_going.pop()
            if node.left:
                stack_going.append(node.left)
            stack_returning.append(node)

        elif stack_returning:
            node = stack_returning.pop()
            if node.right:
                stack_going.append(node.right)
            yield node


def postorder_non_rec(tree):
    stack = []
    stack.append(tree)
    stack_returning = []

    while stack:
        if stack:
            node = stack.pop
            if node.right:
                stack.append(node.right)
            if node.left:
                stack.append(node.left)
            stack_returning.append(node)
        elif stack_returning:
            node = stack_returning.pop()
            yield node


# P1: 1
# P2: 7 5

#ida:
#volta:

#


#import ipdb; ipdb.set_trace()
for t in [tree0, tree1, tree2]:
    print("new tree:")
    for i in inorder_non_rec(t):
        print(i)




#       7
#     /  \
#    /    \
#   5      1
#  / \    / \
# 3   8  0   6

# 3 8 5 0 6 1 7