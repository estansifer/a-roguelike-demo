
import union_find
import random

# The tiles are features of the level;
# every square in the level is exactly one of these.

tile_wall = ' '
tile_floor = '.'
tile_hallway = '#'
tile_vert_wall = '|'
tile_horz_wall = '-'
tile_doorway = '+'

room_height_min = 4
room_width_min = 4

num_rooms_wide = 5
num_rooms_tall = 4

duplicate_hallways = True

def is_passable(tile):
    return tile in [tile_floor, tile_hallway, tile_doorway]

class Level:
    def __init__(self, width, height):
        if width < num_rooms_wide * (room_width_min + 1) - 1:
            print "Level too narrow"
            return
        if height < num_rooms_tall * (room_height_min + 1) - 1:
            print "Level too short"
            return

        self.width = width
        self.height = height
        self.tiles = [None] * height
        for i in xrange(0, height):
            self.tiles[i] = [tile_wall] * width

        self.make_level()

    def make_level(self):
        room_dims = [None] * num_rooms_tall
        for i in xrange(0, num_rooms_tall):
            room_dims[i] = [None] * num_rooms_wide

        for i in xrange(0, num_rooms_tall):
            for j in xrange(0, num_rooms_wide):
                x1 = (j * self.width) / num_rooms_wide
                x2 = ((j + 1) * self.width) / num_rooms_wide - 3
                y1 = (i * self.height) / num_rooms_tall
                y2 = ((i + 1) * self.height) / num_rooms_tall - 3
                room_dims[i][j] = self.place_room_within_bounds(x1, y1, x2, y2)

        east_links = [None] * num_rooms_tall
        south_links = [None] * num_rooms_tall
        for i in xrange(0, num_rooms_tall):
            east_links[i] = [False] * num_rooms_wide
            south_links[i] = [False] * num_rooms_wide

        n = num_rooms_wide * num_rooms_tall

        u = union_find.UnionFind(n)

        while not u.is_connected():
            if (not (num_rooms_wide is 1)) and ((num_rooms_tall is 1) or random.randint(0, 1)):
                # East hallway
                i = random.randint(0, num_rooms_tall - 1)
                j = random.randint(0, num_rooms_wide - 2)
                if duplicate_hallways or (not east_links[i][j]):
                    east_links[i][j] = True

                    rn = i * num_rooms_wide + j
                    u.union(rn, rn + 1)

                    x1, y1, x2, y2 = room_dims[i][j]
                    x1_, y1_, x2_, y2_ = room_dims[i][j + 1]
                    self.__place_east_hallway(x2, y1, y2, x1_, y1_, y2_)

            else:
                # South hallway
                i = random.randint(0, num_rooms_tall - 2)
                j = random.randint(0, num_rooms_wide - 1)

                if duplicate_hallways or (not south_links[i][j]):
                    south_links[i][j] = True

                    rn = i * num_rooms_wide + j
                    u.union(rn, rn + num_rooms_wide)

                    x1, y1, x2, y2 = room_dims[i][j]
                    x1_, y1_, x2_, y2_ = room_dims[i + 1][j]
                    self.__place_south_hallway(x1, x2, y2, x1_, x2_, y1_)


    #
    # Given bounds within which, place a room.
    # We randomly choose a size within those bounds,
    # and randomly choose a room type.
    # We return the actual room dimensions.
    #
    # Minimum room size is 3 by 3.
    #
    # x1, y1 -- inclusive
    # x2, y2 -- exclusive
    #

    def place_room_within_bounds(self, x1, y1, x2, y2):
        x1_, y1_, x2_, y2_ = 0, 0, 0, 0

        while x2_ - x1_ < room_width_min:
            x1_ = random.randint(x1, x2 - 1)
            x2_ = random.randint(x1, x2 - 1)

        while y2_ - y1_ < room_height_min:
            y1_ = random.randint(y1, y2 - 1)
            y2_ = random.randint(y1, y2 - 1)

        self.__place_room_1(x1_, y1_, x2_, y2_)

        return x1_, y1_, x2_, y2_

    def __place_room_1(self, x1, y1, x2, y2):
        for i in xrange(y1 + 1, y2 - 1):
            for j in xrange(x1 + 1, x2 - 1):
                self.tiles[i][j] = tile_floor

        for i in xrange(y1 + 1, y2 - 1):
            self.tiles[i][x1] = tile_vert_wall
            self.tiles[i][x2 - 1] = tile_vert_wall

        for i in xrange(x1, x2):
            self.tiles[y1][i] = tile_horz_wall
            self.tiles[y2 - 1][i] = tile_horz_wall

    def __place_east_hallway(self, x2, y1, y2, x1_, y1_, y2_):
        start_y = random.randint(y1 + 1, y2 - 2)
        end_y = random.randint(y1_ + 1, y2_ - 2)
        turn_x = random.randint(x2, x1_ - 1)

        self.tiles[start_y][x2 - 1] = tile_doorway
        for i in xrange(x2, turn_x):
            self.tiles[start_y][i] = tile_hallway

        if start_y < end_y:
            for i in xrange(start_y, end_y):
                self.tiles[i][turn_x] = tile_hallway
        else:
            for i in xrange(start_y, end_y, -1):
                self.tiles[i][turn_x] = tile_hallway

        for i in xrange(turn_x, x1_):
            self.tiles[end_y][i] = tile_hallway
        self.tiles[end_y][x1_] = tile_doorway

    def __place_south_hallway(self, x1, x2, y2, x1_, x2_, y1_):
        start_x = random.randint(x1 + 1, x2 - 2)
        end_x = random.randint(x1_ + 1, x2_ - 2)
        turn_y = random.randint(y2, y1_ - 1)

        self.tiles[y2 - 1][start_x] = tile_doorway
        for i in xrange(y2, turn_y):
            self.tiles[i][start_x] = tile_hallway

        if start_x < end_x:
            for i in xrange(start_x, end_x):
                self.tiles[turn_y][i] = tile_hallway
        else:
            for i in xrange(start_x, end_x, -1):
                self.tiles[turn_y][i] = tile_hallway

        for i in xrange(turn_y, y1_):
            self.tiles[i][end_x] = tile_hallway
        self.tiles[y1_][end_x] = tile_doorway

    def __place_west_hallway(self, x1, y1, y2, x2_, y1_, y2_):
        self.__place_east_hallway(x2_, y1_, y2_, x1, y1, y2)

    def __place_north_hallway(self, x1, x2, y1, x1_, x2_, y2_):
        self.__place_south_hallway(x1_, x2_, y2_, x1, x2, y1)
