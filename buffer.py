#!/usr/bin/env python3

from PyQt6 import QtCore, QtGui, QtWidgets
from PyQt6.QtGui import QColor, QBrush
from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtWidgets import QApplication
import random

TILE_COLORS = [
    0x999999,
    0xEEE4DA,
    0xEDE0C8,
    0xF2B179,
    0xF59563,
    0xF67C5F,
    0xF65E3B,
    0xEDCF72,
    0xEDCC61,
    0xEDC850,
    0xEDC53F,
    0xEDC22E,
]

class NumberBuffer():
    """ ring buffer """
    def __init__(self, n=2, fill=0):
        self.n = n
        self.fill = fill
        self.clear(fill)

    def append(self, num):
        self.value.append(num)
        if len(self.value) > self.n:
            self.value.pop(0)

    def clear(self, last=0):
        self.value = [0] * (self.n - 1) + [last]

    def __str__(self):
        return "".join(map(str, self.value))

    def __int__(self):
        return int(str(self))

    def __eq__(self, other):
        if type(other) == int:
            return other == int(self)
        return self == other
        

class Tile:
    def __init__(self, value):
        self.value = value


class Game2048(QtWidgets.QWidget):
    def __init__(self, parent, max_width=8000, gridSize=4):
        QtWidgets.QWidget.__init__(self, parent)
        self.game_code = NumberBuffer(fill=gridSize)
        self.gameRunning = False
        self.panelHeight = 80
        self.backgroundBrush = QtGui.QBrush(QtGui.QColor(0xBBADA0))
        self.gridSize = gridSize
        self.post_msg = f"Press r to restart {gridSize}x{gridSize} game, c to clear"
        self.max_width = max_width
        self.tileMargin = 16
        self.gridOffsetX = self.tileMargin
        self.gridOffsetY = self.panelHeight + self.tileMargin
        self.brushes = {
            2 ** i: QBrush(QColor(color)) for i, color in enumerate(TILE_COLORS)
        }
        self.brushes[0] = QBrush(QColor(0xCDC1B4))
        self.lightPen = QtGui.QPen(QColor(0xF9F6F2))
        self.darkPen = QtGui.QPen(QColor(0x776E65))
        self.scoreRect = QtCore.QRect(10, 10, 80, self.panelHeight - 20)
        self.hiScoreRect = QtCore.QRect(100, 10, 80, self.panelHeight - 20)
        self.post = QtCore.QRectF(190, 10, 500, self.panelHeight - 20)
        self.scoreLabel = QtCore.QRectF(10, 25, 80, self.panelHeight - 30)
        self.hiScoreLabel = QtCore.QRectF(100, 25, 80, self.panelHeight - 30)
        self.hiScore = 0
        self.lastPoint = None
        self.reset_game()
        self.setWindowTitle("2048 H")

    def resizeEvent(self, e):
        width = min(e.size().width(),
                    e.size().height() - self.panelHeight,)
        self.tileSize = (width - self.tileMargin * (self.gridSize + 1)) / self.gridSize
        self.font = QtGui.QFont("Arial", self.tileSize / 4)

    def reset_game(self):
        code = int(self.game_code) 
        if code > 0:
            self.gridSize = code
        self.tiles = [
            [None] * self.gridSize for i in range(0, self.gridSize)
        ]
        self.availableSpots = list(range(0, self.gridSize * self.gridSize))
        # self.resize(QtCore.QSize(self.width, self.width + self.panelHeight))
        self.score = 0
        self.addTile()
        self.addTile()
        self.update()
        self.gameRunning = True
        width = self.frameGeometry().width()
        height = self.frameGeometry().height()
        self.resize(QtCore.QSize(min(width, self.max_width),
                                 min(height, self.max_width)))
        self.move(0, 0)

    def addTile(self):
        if len(self.availableSpots) > 0:
            v = 2 if random.random() < 0.9 else 4
            i = self.availableSpots.pop(int(random.random() * len(self.availableSpots)))
            gridX = i % self.gridSize
            gridY = i // self.gridSize
            self.tiles[gridX][gridY] = Tile(v)

    def up(self):
        moved = False
        for gridX in range(0, self.gridSize):
            for gridY in range(1, self.gridSize):
                if self.tiles[gridX][gridY] is not None:
                    i = gridY
                    while i - 1 >= 0 and self.tiles[gridX][i - 1] is None:
                        i -= 1
                    if (
                        i - 1 >= 0
                        and self.tiles[gridX][i - 1].value
                        == self.tiles[gridX][gridY].value
                    ):
                        self.score += self.tiles[gridX][gridY].value * 2
                        self.tiles[gridX][i - 1].value *= 2
                        self.tiles[gridX][gridY] = None
                        moved = True
                    elif i < gridY:
                        self.tiles[gridX][i] = self.tiles[gridX][gridY]
                        self.tiles[gridX][gridY] = None
                        moved = True
        if moved:
            self.updateTiles()

    def down(self):
        moved = False
        for gridX in range(0, self.gridSize):
            for gridY in range(self.gridSize - 2, -1, -1):
                if self.tiles[gridX][gridY] is not None:
                    i = gridY
                    while i + 1 < self.gridSize and self.tiles[gridX][i + 1] is None:
                        i += 1
                    if (
                        i + 1 < self.gridSize
                        and self.tiles[gridX][i + 1].value
                        == self.tiles[gridX][gridY].value
                    ):
                        self.score += self.tiles[gridX][gridY].value * 2
                        self.tiles[gridX][i + 1].value *= 2
                        self.tiles[gridX][gridY] = None
                        moved = True
                    elif i > gridY:
                        self.tiles[gridX][i] = self.tiles[gridX][gridY]
                        self.tiles[gridX][gridY] = None
                        moved = True
        if moved:
            self.updateTiles()

    def left(self):
        moved = False
        for gridX in range(1, self.gridSize):
            for gridY in range(0, self.gridSize):
                if self.tiles[gridX][gridY] is not None:
                    i = gridX
                    while i - 1 >= 0 and self.tiles[i - 1][gridY] is None:
                        i -= 1
                    if (
                        i - 1 >= 0
                        and self.tiles[i - 1][gridY].value
                        == self.tiles[gridX][gridY].value
                    ):
                        self.score += self.tiles[gridX][gridY].value * 2
                        self.tiles[i - 1][gridY].value *= 2
                        self.tiles[gridX][gridY] = None
                        moved = True
                    elif i < gridX:
                        self.tiles[i][gridY] = self.tiles[gridX][gridY]
                        self.tiles[gridX][gridY] = None
                        moved = True
        if moved:
            self.updateTiles()

    def right(self):
        moved = False
        for gridX in range(self.gridSize - 2, -1, -1):
            for gridY in range(0, self.gridSize):
                if self.tiles[gridX][gridY] is not None:
                    i = gridX
                    while i + 1 < self.gridSize and self.tiles[i + 1][gridY] is None:
                        i += 1
                    if (
                        i + 1 < self.gridSize
                        and self.tiles[i + 1][gridY].value
                        == self.tiles[gridX][gridY].value
                    ):
                        self.score += self.tiles[gridX][gridY].value * 2
                        self.tiles[i + 1][gridY].value *= 2
                        self.tiles[gridX][gridY] = None
                        moved = True
                    elif i > gridX:
                        self.tiles[i][gridY] = self.tiles[gridX][gridY]
                        self.tiles[gridX][gridY] = None
                        moved = True
        if moved:
            self.updateTiles()

    def updateTiles(self):
        self.availableSpots = []
        for i in range(0, self.gridSize):
            for j in range(0, self.gridSize):
                if self.tiles[i][j] is None:
                    self.availableSpots.append(i + j * self.gridSize)
        self.addTile()
        self.hiScore = max(self.score, self.hiScore)
        self.update()

    def movesAvailable(self):
        if not len(self.availableSpots) == 0:
            return True
        for i in range(0, self.gridSize):
            for j in range(0, self.gridSize):
                if (
                    i < self.gridSize - 1
                    and self.tiles[i][j].value == self.tiles[i + 1][j].value
                ):
                    return True
                if (
                    j < self.gridSize - 1
                    and self.tiles[i][j].value == self.tiles[i][j + 1].value
                ):
                    return True
        return False

    def keyPressEvent(self, e):
        if e.key() in [Qt.Key.Key_R]:
            self.reset_game()
        elif e.key() in range(0x30, 0x40):  # number keys
            self.game_code.append(e.key() - 48)  # 48 is the keycode of 0 key
            self.update()
        elif e.key() in [Qt.Key.Key_Escape, Qt.Key.Key_C]:
            self.game_code.clear()
            self.update()
        if not self.gameRunning:
            return
        elif e.key() in [Qt.Key.Key_Up, Qt.Key.Key_K]:
            self.up()
        elif e.key() in [Qt.Key.Key_Down, Qt.Key.Key_J]:
            self.down()
        elif e.key() in [Qt.Key.Key_Left, Qt.Key.Key_H]:
            self.left()
        elif e.key() in [Qt.Key.Key_Right, Qt.Key.Key_L]:
            self.right()

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        painter.setPen(QtCore.Qt.PenStyle.NoPen)
        painter.setBrush(self.backgroundBrush)
        painter.drawRect(self.rect())
        painter.setBrush(self.brushes[1])
        painter.drawRoundedRect(self.scoreRect, 10.0, 10.0)
        painter.drawRoundedRect(self.hiScoreRect, 10.0, 10.0)
        painter.drawRoundedRect(self.post, 10.0, 10.0)
        painter.setFont(QtGui.QFont("Arial", 9))
        painter.setPen(self.darkPen)
        painter.drawText(
            QtCore.QRectF(10, 15, 80, 20),
            "SCORE",
            QtGui.QTextOption(
                QtCore.Qt.AlignmentFlag.AlignHCenter
                | QtCore.Qt.AlignmentFlag.AlignVCenter
            ),
        )
        painter.drawText(
            QtCore.QRectF(100, 15, 80, 20),
            "HIGHSCORE",
            QtGui.QTextOption(
                QtCore.Qt.AlignmentFlag.AlignHCenter
                | QtCore.Qt.AlignmentFlag.AlignVCenter
            ),
        )
        painter.setFont(QtGui.QFont("Arial", 15))
        painter.setPen(self.lightPen)
        size = int(self.game_code)
        if not self.movesAvailable():
            self.post_msg = f"Game Over, Press r to restart a {size}x{size} game"
            self.gameRunning = False
        else:
            self.post_msg = f"Press r to restart a {size}x{size} game, c to clear"
        painter.drawText(
            self.post,
            self.post_msg,
            QtGui.QTextOption(
                QtCore.Qt.AlignmentFlag.AlignHCenter
                | QtCore.Qt.AlignmentFlag.AlignVCenter
            ),
        )
        painter.setFont(QtGui.QFont("Arial", 15))
        painter.setPen(self.lightPen)
        painter.drawText(
            self.scoreLabel,
            str(self.score),
            QtGui.QTextOption(
                QtCore.Qt.AlignmentFlag.AlignHCenter
                | QtCore.Qt.AlignmentFlag.AlignVCenter
            ),
        )
        painter.drawText(
            self.hiScoreLabel,
            str(self.hiScore),
            QtGui.QTextOption(
                QtCore.Qt.AlignmentFlag.AlignHCenter
                | QtCore.Qt.AlignmentFlag.AlignVCenter
            ),
        )
        painter.setFont(self.font)
        for gridX in range(0, self.gridSize):
            for gridY in range(0, self.gridSize):
                tile = self.tiles[gridX][gridY]
                if tile is None:
                    painter.setBrush(self.brushes[0])
                else:
                    painter.setBrush(self.brushes[tile.value])
                rect = QtCore.QRectF(
                    self.gridOffsetX + gridX * (self.tileSize + self.tileMargin),
                    self.gridOffsetY + gridY * (self.tileSize + self.tileMargin),
                    self.tileSize,
                    self.tileSize,
                )
                painter.setPen(QtCore.Qt.PenStyle.NoPen)
                painter.drawRoundedRect(rect, 10.0, 10.0)
                if tile is not None:
                    painter.setPen(self.darkPen if tile.value < 16 else self.lightPen)
                    painter.drawText(
                        rect,
                        str(tile.value),
                        QtGui.QTextOption(
                            QtCore.Qt.AlignmentFlag.AlignHCenter
                            | QtCore.Qt.AlignmentFlag.AlignVCenter
                        ),
                    )

def copy_tiles_to_grid(tiles):
    n = len(tiles)
    grid = [[None] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            if tiles[i][j]:
                grid[i][j] = tiles[i][j].value
    return grid


class GameAuto(Game2048):
    def __init__(self, parent, max_width=8000, gridSize=4):
        Game2048.__init__(self, parent, max_width, gridSize)
        self.auto_type = "ai"
        self.mytimer = QTimer(self)
        self.mytimer.timeout.connect(self.onTimer)
        self.mytimer.start(200)
        self.ai = AI(self.tiles)

    def keyPressEvent(self, e):
        pass

    def onTimer(self):
        if self.auto_type == "random":
            act = random.choice(["up", "down",  "left", "right"])
        else:
            act = self.ai.getNextMove(copy_tiles_to_grid(self.tiles))
        getattr(self, act)()


if __name__ == "__main__":
    import sys
    from minmax import AI
    app = QtWidgets.QApplication([])
    size = 4 if len(sys.argv) == 1 else int(sys.argv[1])

    if size == 0:
        g = GameAuto(None, 8000, 4)
    else:
        g = Game2048(None, 8000, size)
    g.show()
    app.exec()
else:
    from core.buffer import Buffer
    import importlib
    mm = importlib.import_module('app.2048.minmax')
    AI = mm.AI

    class AppBuffer(Buffer):
        def __init__(self, buffer_id, url, arguments):
            Buffer.__init__(self, buffer_id, url, arguments, False)
            gridsize = 4
            try:
                gridsize = int(arguments)
            except:
                pass
            if gridsize == 0:
                self.add_widget(GameAuto(None, 1000, 4))
            else:
                self.add_widget(Game2048(None, 1000, gridsize))
