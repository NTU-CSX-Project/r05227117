import json
import os
import numpy as np
import cv2
from darkflow.net.build import TFNet
import glob as gb

###########################################################################################################

####run under windows system

def video_to_frames(video, path_ouput_dir):
    vidcap=cv2.VideoCapture(video)
    count=0
    while vidcap.isOpened():
        succes, image = vidcap.read()
        if succes:
            cv2.imwrite(os.path.join(path_ouput_dir, '%d.jpg') % count, image)
            count+=1
        else:
            break
    cv2.destroyAllWindows()
    vidcap.release()

###########################################################################################################

####run under linux system


options = {"model": "cfg/yolo.cfg",
           "load": "bin/yolo.weights",
           "threshold": 0.3}
tfnet = TFNet(options)
#os.getcwd()
video_to_frames("2.mp4","./picture/")

#change working directory to where the pictures was stored.
os.chdir("./picture")

# load each pictures' filename////  all_picture = list = ['1.jpg','2.jpg','3.jpg',...]  
all_picture = gb.glob('*.jpg') 

#for loop to process each picture
for i in range(len(all_picture)):
    img = cv2.imread(all_picture[i])
    predictions = tfnet.return_predict(img)
    for box in predictions:
        box["confidence"] = float(box["confidence"])
    #for loop to process each objection in picture
    for j in range(len(predictions)):
        cv2.rectangle(img,(predictions[j]["topleft"]["x"],predictions[j]["topleft"]["y"]),(predictions[j]["bottomright"]["x"],predictions[j]["bottomright"]["y"]),(0,255,0),5)
        font = cv2.FONT_HERSHEY_SIMPLEX
        cv2.putText(img,str(predictions[j]["label"]),(predictions[j]["topleft"]["x"],predictions[j]["bottomright"]["y"]), font, 3,(255,255,255),5,cv2.LINE_AA)
    os.chdir("./../poutput")
    #write images and name them follow pc000.jpg~pcxxx.jpg
    cv2.imwrite(('pc%03d.jpg')%i,img)
    cv2.destroyAllWindows()
    os.chdir("./../picture") 




##########################################################################################################

####run under windows system

os.chdir("./poutput")
pictures = gb.glob("*.jpg")

#read one of pictures to set the video size
imgsize = cv2.imread(('%s')%pictures[0])
height , width , layers =  imgsize.shape
#set video parameter(filename,code,FPS,size) and create an empty video
fourcc = cv2.VideoWriter_fourcc(*'XVID')
video = cv2.VideoWriter('video1.avi',fourcc,24,(width,height))
#write each frame into video.
for i in range(len(pictures)):
    img = cv2.imread(('pc%03d.jpg')%i)
    video.write(img)
cv2.destroyAllWindows()
video.release()
