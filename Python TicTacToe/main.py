from kivy.app import App
from kivy.uix.widget import Widget
from kivy.uix.label import Label
from kivy.uix.gridlayout import GridLayout
from kivy.uix.button import Button
from kivy.properties import (ListProperty, NumericProperty)
from kivy.uix.modalview import ModalView

class GridEntry(Button):
    coords = ListProperty([0,0])

class TicTacToeGrid(GridLayout):
    status = ListProperty([0, 0, 0, 0, 0, 0, 0, 0, 0])
    currentPlayer = NumericProperty(1)

    def __init__(self, *args, **kwargs):
        super(TicTacToeGrid, self).__init__(*args, **kwargs)
        for row in range(3):
            for column in range(3):
                gridEntry = GridEntry(**{'coords': [row, column]}) #
                gridEntry.bind(**{'on_release': self.buttonPressed}) #
                self.add_widget(gridEntry)

    def buttonPressed(self, button):
        player = {1: 'X', -1: 'O'}
        colors = {1: {0,0,0,0}, -1: {0,0,0,0}}

        row = button.coords[0]
        column = button.coords[1]

        statusIndex = 3 * row + column
        alreadyPlayed = self.status[statusIndex]

        if not alreadyPlayed:
            self.status[statusIndex] = self.currentPlayer
            button.text = player[self.currentPlayer]
            button.background_color = colors[self.currentPlayer]
            self.currentPlayer *= -1

    def reset(self, *args):
        self.status = [0, 0, 0, 0, 0, 0, 0, 0, 0]

        for child in self.children:
            child.text = '' #

        self.currentPlayer = 1

    def on_status(self, instance, newVal):
        status = newVal

        sums = [sum(status[0:3]), # rows
                sum(status[3:6]), sum(status[6:9]), sum(status[0::3]), # columns
                sum(status[1::3]), sum(status[2::3]), sum(status[::4]), # diagonals
                sum(status[2:-2:2])]

        winner = ""
        if 3 in sums:
            winner = 'Xs win!'
        elif -3 in sums:
            winner = 'Os win!'
        elif 0 not in self.status:
            winner = 'Draw!'

        if winner:
            popup = ModalView(**{'size_hint': [.75, .5]}) #
            victoryLabel = Label(**{'text': winner, 'font_size': 50}) #
            popup.add_widget(victoryLabel)
            popup.bind(**{'on_dismiss': self.reset}) #
            popup.open()

class TicTacToeApp(App):
    def build(self):
        return TicTacToeGrid()

if __name__ == '__main__':
    TicTacToeApp().run()
