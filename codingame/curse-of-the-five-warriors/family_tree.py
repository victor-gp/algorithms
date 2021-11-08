from json import dumps, loads
from typing import List


def compute(nodes: List[dict]) -> str:
    '''
    Args:
        - nodes (List[dict]): Each node represents a member of this family. Described by their name, parents, children and spouse.
	As you might expect, this medieval document only acknowledges male/female, and father/mother family units.

    Returns:
        The name on the candle you wish to place in this candle holder.
    '''

    def parents(node_zero: dict):
        parent_ids = []
        if node_zero["fatherId"] != -1:
            parent_ids.append(node_zero["fatherId"])
        if node_zero["motherId"] != -1:
            parent_ids.append(node_zero["motherId"])
        if len(parent_ids) == 0:
            return []
        parent_nodes = []
        for node in nodes:
            if node["id"] in parent_ids:
                parent_nodes.append(node)
                if len(parent_nodes) == 2:
                    return parent_nodes
        return parent_nodes

    def grandparents(node_zero: dict):
        parents_ = parents(node_zero)
        grandparents = [
            grandparent for parent in parents_ for grandparent in parents(parent)
        ]
        return grandparents

    def is_only_child(node_zero: dict):
        parents_ = parents(node_zero)
        if len(parents_) == 0:
            return False
        parent = parents_[0]
        if len(parent["childIds"]) == 1:
            return True
        else:
            return False

    def children(node_zero: dict):
        children_ids = node_zero["childIds"]
        if len(children_ids) == 0:
            return []
        children_nodes = []
        for node in nodes:
            if node["id"] in children_ids:
                children_nodes.append(node)
        return children_nodes

    def ndescendants(node_zero: dict):
        children_ = children(node_zero)
        ndescendants_ = len(children_)
        for child in children_:
            ndescendants_ += ndescendants(child)
        return ndescendants_

    for node in nodes:
        if node["gender"] != "F":
            continue
        if len(node["childIds"]) != 1:
            continue
        grandparents = grandparents(node)
        for gp in grandparents:
            if is_only_child(gp):
                print(node)
                break

    for node in nodes:
        if node["gender"] != "M":
            continue
        if ndescendants(node) != 48:
            continue
        print(node)


# Ignore and do not change the code below
#region main


def try_solution(name: str):
    '''
    Try a solution

    Args:

        - str (str): The name on the candle you wish to place in this candle holder.
    '''
    json = name
    print("@@solution@@:" + dumps(json))

def main():
    res = compute(
        loads(input())
    )
    try_solution(res)


if __name__ == "__main__":
    main()
#endregion
