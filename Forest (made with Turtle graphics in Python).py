# project01, Sebastian Naibaho

from drawings import *
import turtle
import math
import random

def drawScene():
    # drawTree()
    pass

def drawTree(t, height, green_color):
    drawRectangle(t,10,0.3*height,0,"brown","brown") # 3drawTriangle(t, (0.5*height)/math.sqrt(3), green_color, green_color) # 1
    t.up()
    t.left(90)
    t.forward(0.3*height) # 3
    t.right(90)
    t.backward((0.3*height/math.sqrt(3))-5) # 2
    t.down()
    drawTriangle(t,(0.6*height)/math.sqrt(3), green_color, green_color)
    t.up()
    t.left(90)
    t.forward(0.2*height)
    t.down()
    drawTriangle(t,(0.6*height)/math.sqrt(3), green_color, green_color)
    t.up()
    t.left(90)
    t.forward(0.2*height)
    t.down()
    drawTriangle(t,(0.6*height)/math.sqrt(3), green_color, green_color)


def checkTreeHeight(t):
    t.up()
    t.goto(0,-200)
    t.down()
    drawRectangle(t, 200, 200, 0 , "red","")
    t.seth(0)
    drawTree(t, 200, "green")

def drawForest(t):

    for i in range(5):
        x=random.randrange(-300,501)
        y=random.randrange(100,150)
        t.up()
        t.seth(0)
        t.goto(x, y)
        t.down()
        drawTree(t, 200, "green")

    for i in range(5):
        x=random.randrange(-300,501)
        y=random.randrange(100,150)
        t.up()
        t.seth(0)
        t.goto(x, y)
        t.down()
        drawTree(t, 200, '#7CFC00')

    for i in range(1):
        x=random.randrange(-300,501)
        y=random.randrange(100,150)
        t.up()
        t.seth(0)
        t.goto(x, y)
        t.down()
        drawTree(t, 200, "#8FBC8F")

    for i in range(4):
        x=random.randrange(-300,501)
        y=random.randrange(100,150)
        t.up()
        t.seth(0)
        t.goto(x, y)
        t.down()
        drawTree(t, 200, "#20B2AA")

def drawHut(t):
    for y in range(20):
        drawRectangle(t,5, 50,0,"black","brown")
        t.forward(4)
  
    t.up()
    t.seth(0)
    t.left(90)
    t.forward(70)
    t.left(90)
    t.forward(40.5)
    t.down()

    for tilt in range(-250,-100,10):
        drawRectangle(t,5, 50,tilt,"black","brown")

def drawVillage(t):
    width=100
    x=-275

    for i in range(5):
        x += width
        y=random.randrange(-150,-100)
        t.up()
        t.seth(0)
        t.goto(x, y)
        t.down()
        drawHut(t)

def Sun(t):
    '''
    A circle of triangles
    '''
    t.pensize(0)
    for i in range(36):
        t.up()
        radius = 100
        x = (radius*math.cos(i*10*math.pi/180))-500
        y = (radius*math.sin(i*10*math.pi/180))+250
        t.goto(x,y)
        t.down()
        drawTriangle(t, 20, "black", "yellow")

def Sun_2(t):
    '''
    A circle of triangles
    '''
    t.pensize(0)
    for i in range(36):
        t.up()
        radius = 40
        x = (radius*math.cos(i*10*math.pi/180))-493
        y = (radius*math.sin(i*10*math.pi/180))+256
        t.goto(x,y)
        t.down()
        drawTriangle(t, 10, "black", "yellow")

def Sun_3(t):
    '''
    A circle of triangles
    '''
    t.pensize(0)
    for i in range(36):
        t.up()
        radius = 70
        x = (radius*math.cos(i*10*math.pi/180))-496
        y = (radius*math.sin(i*10*math.pi/180))+252.5
        t.goto(x,y)
        t.down()
        drawTriangle(t, 15, "black", "yellow")


if __name__=="__main__":
    print('Inside main of project01.py')
    Jane = turtle.Turtle()
    Jane.speed(0)
    drawForest(Jane)
    drawVillage(Jane)
    Sun(Jane)
    Sun_2(Jane)
    Sun_3(Jane)
    # drawHut(Jane)
    # drawScene(Jane)
