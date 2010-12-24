
class UnionFind:
    def __init__(self, n):
        self.n = n
        self.groups = range(0, n)

    def union(self, i, j):
        self.groups[self.__root(i)] = self.__root(j)

    def is_connected(self):
        r = self.__root(0)
        for i in xrange(1, self.n):
            if self.__root(i) is not r:
                return False
        return True
        
    def __root(self, i):
        if self.groups[i] is i:
            return i
        else:
            j = self.__root(self.groups[i])
            self.groups[i] = j
            return j
