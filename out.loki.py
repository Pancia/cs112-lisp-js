class Loki:
    @staticmethod
    def printf(x):
        print(x)
    @staticmethod
    def plus(*args):
        return reduce((lambda x, y : x + y), args)
    @staticmethod
    def minus(*args):
        return reduce((lambda x, y : x - y), args)
    @staticmethod
    def div(*args):
        return reduce((lambda x, y : x / y), args)
    @staticmethod
    def mult(*args):
        return reduce((lambda x, y : x * y), args)
    @staticmethod
    def and_(*args):
        return reduce((lambda x, y : x and y), args)
    @staticmethod
    def or_(*args):
        return reduce((lambda x, y : x or y), args)
    @staticmethod
    def eq(*args):
        return not not reduce((lambda x, y : x if x == y else False), args)
    @staticmethod
    def neq(*args):
        return not Loki.eq(*args)
    @staticmethod
    def lt(*args):
        return not not reduce((lambda x, y : y if x < y else False), args)
    @staticmethod
    def lte(*args):
        return not not reduce((lambda x, y : y if x <= y else False), args)
    @staticmethod
    def gt(*args):
        return not not reduce((lambda x, y : y if x > y else False), args)
    @staticmethod
    def gte(*args):
        return not Loki.lt(*args)
    @staticmethod
    def mod(x, y):
        return x % y
    @staticmethod
    def range(n):
        return range(n)
    @staticmethod
    def get(e, i):
        return e[i]
    @staticmethod
    def set(x, v):
        x = v
    @staticmethod
    def assoc(x, i, v):
        x[i] = v
        return x
    @staticmethod
    def in_(x, l):
        return (x in l)
    @staticmethod
    def sc(n, x, l):
        return n[x:l]
    @staticmethod
    def dc(n, x, l):
        return n[x::l]
    @staticmethod
    def dcm(n, x, m, l):
        return n[x:m:l]
    @staticmethod
    def not_ (x):
        return not x

#END LOKI HELPER FUNCTIONS
class GridEntry(Button):
    def __init__(self):
        self.coords = ListProperty([0, 0])


def t1 (self):
    Loki.assoc(self-status, statusIndex, (self.currentPlayer() if callable(self.currentPlayer) else self.currentPlayer))
    button.text = Loki.get(player, (self.currentPlayer() if callable(self.currentPlayer) else self.currentPlayer))
    button.background_color = Loki.get(colors, (self.currentPlayer() if callable(self.currentPlayer) else self.currentPlayer))
    self.currentPlayer = Loki.mult((self.currentPlayer() if callable(self.currentPlayer) else self.currentPlayer), -1)
    return False
class TicTacToeGrid(GridLayout):
    def __init__(self, *args, **kwargs):
        self.status = ListProperty([0, 0, 0, 0, 0, 0, 0, 0, 0])
        self.currentPlayer = NumericProperty(1)
        self.cols = 3
        for row in Loki.range(3):
            for col in Loki.range(3):
                _coords = {"coords" : (row,col)}
                gridEntry = GridEntry(**_coords)
                _button_pressed = (self.buttonPressed() if callable(self.buttonPressed) else self.buttonPressed)
                _on_release = {"on_release" : _button_pressed}
                (gridEntry.bind(**_on_release) if callable(gridEntry.bind) else gridEntry.bind)
                (self.add_widget(gridEntry) if callable(self.add_widget) else self.add_widget)
    def buttonPressed (self, self, button):
        return do(player = {-1 : "0", 1 : "X"}, colors = {-1 : (0,0,0,0), 1 : (0,0,0,0)}, row = Loki.get(button, Loki.get(coords, 0)), column = Loki.get(button, Loki.get(coords, 1)), statusIndex = Loki.plus(Loki.mult(Loki.get(row, 3)), column), alreadyPlayed = Loki.get(self, Loki.get(status, statusIndex)), (t1(self) if Loki.not_(alreadyPlayed) else None))

    def reset (self, self, *args):
        return (lambda  : [self.status = [0, 0, 0, 0, 0, 0, 0, 0, 0], for child in (self.children() if callable(self.children) else self.children):
    child.text = "", self.currentPlayer = 1])()

    def on_status (self, self, instance, newVal):
        return do(status = newVal, sums = [Loki.plus(Loki.sc(0, 3)), Loki.plus(Loki.sc(3, 6)), Loki.plus(Loki.sc(6, 9)), Loki.plus(Loki.dc(0, 3)), Loki.plus(Loki.dc(1, 4)), Loki.plus(Loki.dc(2, 3)), Loki.plus(Loki.dc(0, 4)), Loki.plus(Loki.dcm(2, -2, 2))], winner = "", (winner = "Xs win!" if Loki.in_(3, sums) else None), (winner = "Os win!" if Loki.in_(-3, sums) else None), (winner = "Draw!" if !in(0, (self.status() if callable(self.status) else self.status)) else None), ((lambda  : [popup = ModalView(**, {"size_hunt" : [Loki.div(3, 4), Loki.div(1, 2)]}), victoryLabel = Label(**, {"font_size" : 50, "text" : winner}), (popup.add_widget(victoryLabel) if callable(popup.add_widget) else popup.add_widget), (popup.bind(**, {"on_dismiss" : (self.reset() if callable(self.reset) else self.reset)}) if callable(popup.bind) else popup.bind), (popup.open() if callable(popup.open) else popup.open)])() if Loki.neq(winner, "") else None))


((TicTacToeApp().run() if callable(TicTacToeApp().run) else TicTacToeApp().run) if Loki.eq(__name__("__main__")) else None)