# from https://github.com/kcwu/2048-python
import random
import sys

range4 = range(4)
N = 4

KEY_CODE = {'left': 37,
            'up': 38,
            'right': 39,
            'down': 40}
KEY_LEFT = 'left'
KEY_UP = 'up'
KEY_RIGHT = 'right'
KEY_DOWN = 'down'


class Board(object):
    def __init__(self):
        self.board = [[None] * N for i in range(N)]
        self.score = 0
        self.over = False
  
    def rotateLeft(self, grid):
        out = self.emptyGrid()
        for c in range(4):
            for r in range(4):
                out[r][3-c] = grid[c][r]
        return out
  
    def rotateRight(self, grid):
        out = self.emptyGrid()
        for c in range(4):
            for r in range(4):
                out[3-r][c] = grid[c][r]
        return out
  
    def emptyGrid(self):
        out = list()
        for x in xrange(4):
            col = list()
            for y in xrange(4):
                col.append(None)
            out.append(col)
        return out
  
    def to_move(self, grid, direction):
        out = self.emptyGrid()
  
        if direction == KEY_UP:
            rot = 1
        elif direction == KEY_RIGHT:
            rot = 2
        elif direction == KEY_DOWN:
            rot = 3
        else:
            rot = 0
  
        for i in xrange(rot):
            grid = self.rotateLeft(grid)
  
        score = 0
        for r in xrange(4):
            oc = 0
            ic = 0
            while ic < 4:
                if grid[ic][r] is None:
                    ic += 1
                    continue
                out[oc][r] = grid[ic][r]
                oc += 1
                ic += 1
  
            ic = 0
            oc = 0
            while ic < 4:
                if out[ic][r] is None:
                    break
                if ic == 3:
                    out[oc][r] = out[ic][r]
                    oc += 1
                    break
                if out[ic][r] == out[ic+1][r]:
                    #out[oc][r] *= 2
                    out[oc][r] = 2*out[ic][r]
                    score += out[oc][r]
                    ic += 1
                else:
                    out[oc][r] = out[ic][r]
                ic += 1
                oc += 1
            while oc < 4:
                out[oc][r] = None
                oc += 1
  
        for i in xrange(rot):
            out = self.rotateRight(out)
  
        return out, score
  
    def move(self, direction):
        #print 'move', direction
        next_board, got_score = self.to_move(self.board, direction)
        moved = (next_board != self.board)
  
        self.board = next_board
        self.score += got_score
  
        if moved:
            if not self.randomTile():
                self.over = True
  
    def canMove(self, grid, direction):
        return grid != self.to_move(grid, direction)[0]
  
    def get_empty_cells(self):
        for i in range(N):
            for j in range(N):
                if self.board[i][j] is None:
                    yield i, j
  
    def randomTile(self):
        cells = list(self.get_empty_cells())
        if not cells:
            return False
        #print 'cells', cells
  
        if random.random() < 0.9:
            v = 2
        else:
            v = 4
  
        cid = random.choice(cells)
        #print cid
        self.board[cid[0]][cid[1]] = v
        return True
  
class GameManager(object):
    def __init__(self):
        self.player = ''
        self.ai = AI()
        self.board = Board()
        self.board.randomTile()
        self.board.randomTile()
  
    def setPlayer(self, name):
        self.player = name
  
    def getGameState(self):
        d = {}
  
        cells = []
        d['grid'] = {'cells': cells}
        for i in range(N):
            row = []
            for j in range(N):
                if self.board.board[i][j]:
                    cell = { 'value': self.board.board[i][j] }
                else:
                    cell = None
  
                row.append(cell)
            cells.append(row)
  
        d['won'] = False # i'm lazy
        d['over'] = self.board.over
  
        return d
  
    def getGrid(self):
        gs = self.getGameState()
        if gs is None:
            return None
        raw_grid = gs['grid']['cells']
        grid = list()
        for i in xrange(4):
            col = [x['value'] if x else None for x in raw_grid[i]]
            grid.append(col)
        return grid
  
    def getScore(self):
        return self.board.score
  
    def isLost(self):
        return self.board.over
  
    def isWin(self):
        return False

    def pressKey(self, kc):
        #print 'pressKey', kc
        if kc == KEY_CODE['left']:
            self.board.move(KEY_LEFT)
        elif kc == KEY_CODE['right']:
            self.board.move(KEY_RIGHT)
        elif kc == KEY_CODE['up']:
            self.board.move(KEY_UP)
        elif kc == KEY_CODE['down']:
            self.board.move(KEY_DOWN)
        else:
            raise ValueError

    def keepGoing(self):
        pass

    def isOver(self):
        return self.board.over


def rotateRight(grid):
    return [[grid[r][3-c] for r in range4] for c in range4]

def move_row(row):
    out = [x for x in row if x]
    ic = oc = 0
    while out[ic:]:
        if out[ic+1:] and out[ic] == out[ic+1]:
            out[oc] = 2*out[ic]
            ic += 1
        else:
            out[oc] = out[ic]
        ic += 1
        oc += 1
    out[oc:]=[None]*(4-oc)
    return out

def move(grid, rot):
    for i in range(rot):
        grid = rotateRight(grid)
    out = list(map(move_row, grid))
    return out, out != grid

def eval_monotone_L(grid):
    L = 0
    for x in range4:
        m = 0
        for y in range(3):
            A = grid[x][y] or 0
            B = grid[x][y+1] or 0
            if A and A >= B:
                m += 1
                L += m ** 2 * 4
            else:
                L -= abs(A- B) * 1.5
                m = 0
    return L

def eval_monotone_LR(grid):
    return max(eval_monotone_L(grid), eval_monotone_L(rotateRight(rotateRight(grid))))

def eval_smoothness(grid):
    return -sum( min([1e8]+[abs((grid[x][y] or 2) - (grid[x+a][y+b] or 2)) for a, b in((-1,0),(0,-1),(1,0),(0,1)) if 0 <= x+a <4 and 0<=y+b<4]) for x in range4 for y in range4)

def count_free(grid):
    return sum(r.count(None) for r in grid)

def EVAL(grid):
    return eval_monotone_LR(grid) +\
        eval_monotone_LR(rotateRight(grid)) +\
        eval_smoothness(grid) - (16-count_free(grid))**2

def search_max(grid):
    return max([EVAL(move(grid,m)[0]) for m in range4 if move(grid,m)[1]]+[-1e8])

def search_min(grid):
    scores = []
    for i in range4:
        row = grid[i]
        for j in range4:
            if not row[j]:
                score = all_p = 0
                for v, p in ((2, 9.), (4, 1)):
                    if count_free(grid) <= 4 or p > 1: # XXX hardcode for level=2
                        row[j] = v
                        score += p * search_max(grid)
                        all_p += p
                row[j] = None

                scores.append(score / all_p)

    return sum(scores) / len(scores)

class AI:
    def getNextMove(self, grid):
        act = max((search_min(move(grid,m)[0]),m) for m in range4 if move(grid,m)[1])[1]
        return ['up','left','down','right'][act]
