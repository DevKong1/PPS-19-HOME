#Building the application
FROM gradle AS TEMP_BUILD_IMAGE
ENV HOME_APP=/usr/app/HOME
COPY --chown=gradle . $HOME_APP
WORKDIR $HOME_APP
RUN gradle ShadowJar

#Launching the application
FROM ubuntu
RUN apt-get update -y && apt-get upgrade -y && apt-get install -y openjdk-8-jdk xvfb x11vnc fluxbox
ENV ARTIFACT_NAME=PPS-19-HOME-all.jar
ENV HOME_APP=/usr/app/HOME
WORKDIR $HOME_APP
COPY --from=TEMP_BUILD_IMAGE $HOME_APP/build/libs/$ARTIFACT_NAME .
ENV DISPLAY :0
CMD Xvfb $DISPLAY -screen 0 1024x768x16 & fluxbox & x11vnc -display $DISPLAY -forever -nopw & java -jar $ARTIFACT_NAME