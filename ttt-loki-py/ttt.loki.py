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
from kivy.app import App
from kivy.uix.widget import Widget
from kivy.uix.label import Label
from kivy.uix.gridlayout import GridLayout
from kivy.uix.button import Button
from kivy.properties import ListProperty
from kivy.properties import NumericProperty
from kivy.uix.modalview import ModalView
class GridEntry(Button):
    coords = ListProperty([0, 0])

def _buttonPressed_helper (self, button, statusIndex, player, colors):
    self.status[statusIndex] = self.currentPlayer
    button.text = Loki.get(player, self.currentPlayer)
    button.background_color = Loki.get(colors, self.currentPlayer)
    self.currentPlayer = Loki.mult(self.currentPlayer, -1)
    return False
def on_win (self, winner):
    _size_hint = {"size_hint" : [0.75, 0.5]}
    popup = ModalView(**_size_hint)
    _victory_label = {"font_size" : 50, "text" : winner}
    victoryLabel = Label(**_victory_label)
    (popup.add_widget(victoryLabel) if callable(popup.add_widget) else popup.add_widget)
    _on_dismiss = {"on_dismiss" : self.reset}
    (popup.bind(**_on_dismiss) if callable(popup.bind) else popup.bind)
    return (popup.open() if callable(popup.open) else popup.open)
class TicTacToeGrid(GridLayout):
    def __init__(self, *args, **kwargs):
        GridLayout.__init__(self,*args,**kwargs)
        for row in Loki.range(3):
                for col in Loki.range(3):
                    _coords = {"coords" : (row,col)}
                    gridEntry = GridEntry(**_coords)
                    _button_pressed = self.buttonPressed
                    _on_release = {"on_release" : _button_pressed}
                    (gridEntry.bind(**_on_release) if callable(gridEntry.bind) else gridEntry.bind)
                    (self.add_widget(gridEntry) if callable(self.add_widget) else self.add_widget)
    status = ListProperty([0, 0, 0, 0, 0, 0, 0, 0, 0])
    currentPlayer = NumericProperty(1)
    def buttonPressed (self, button):
        player = {1 : "X", -1 : "0"}
        colors = {-1 : (0,0,1,0.8), 1 : (1,0,0,0.8)}
        row = Loki.get(button.coords, 0)
        column = Loki.get(button.coords, 1)
        statusIndex = Loki.plus(Loki.mult(row, 3), column)
        _self_status = self.status
        alreadyPlayed = Loki.get(_self_status, statusIndex)
        return (_buttonPressed_helper(self, button, statusIndex, player, colors) if Loki.not_(alreadyPlayed) else None)

    def reset (self, *args):
        self.status = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        for child in self.children:
            child.text = ""
        self.currentPlayer = 1
        return None

    def on_status (self, instance, newVal):
        status = newVal
        sums = [sum(Loki.sc(status, 0, 3)), sum(Loki.sc(status, 3, 6)), sum(Loki.sc(status, 6, 9)), sum(Loki.dc(status, 0, 3)), sum(Loki.dc(status, 1, 3)), sum(Loki.dc(status, 2, 3)), sum(Loki.dc(status, 0, 4)), sum(Loki.dcm(status, 2, -2, 2))]
        winner = ("Xs win!" if Loki.in_(3, sums) else ("Os win!" if Loki.in_(-3, sums) else ("Draw!" if Loki.not_(Loki.in_(0, self.status)) else None)))
        return (on_win(self, winner) if winner else None)


class TicTacToeApp(App):
    def build (self):
        return TicTacToeGrid()


((TicTacToeApp().run() if callable(TicTacToeApp().run) else TicTacToeApp().run) if Loki.eq(__name__, "__main__") else None)
