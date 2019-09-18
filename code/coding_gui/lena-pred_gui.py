from tkinter import *
from tkinter.ttk import *
from playsound import playsound

SND_FILENAME = '../../data/02_processed_data/IDSLabel-norm/IDS/B-12_13_pass1_647_40.wav'
def play(): return playsound(SND_FILENAME)


def key(event):
    print("pressed", repr(event.char))

root = Tk()
root.title('IDSLabel Communicative Intent Coding')
root.geometry("750x500") 
root.resizable(0, 0)  # Don't allow resizing in the x or y direction

button = Button(master=root, text='Play Sound', command=play)
button.pack(side='left')

root.bind_all('<Key>', key)

root.mainloop()
