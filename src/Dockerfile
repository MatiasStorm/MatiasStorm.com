FROM python:3
ENV PYTHONUNBUFFERED 1
RUN mkdir /src
RUN mkdir /complete
WORKDIR /src
ADD . /src/
RUN pip install -r /src/requirements.txt
