FROM luksamuk/roswell-qlot
WORKDIR /app
COPY . .
CMD ["qlot" "install"]

