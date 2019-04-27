FROM luksamuk/roswell-qlot
WORKDIR /app
COPY . .
RUN qlot install

