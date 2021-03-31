FROM python:3.8-slim-buster AS compile-image

RUN apt-get -y update && apt-get -y install make

#COPY requirements.txt .
#RUN pip install -r requirements.txt

# This should be a github repo.
#COPY dafnedset*.whl .
#RUN pip install dafnedset*.whl

RUN mkdir /dload
WORKDIR /dload

COPY dafneloader/ ./dafneloader
COPY pyproject.loader.toml ./pyproject.toml
COPY Makefile .

#RUN git clone https://github.com/jjclavijo/dafnedset.git &&\
RUN make

FROM tensorflow/tensorflow:2.3.1

ENV DAFNE_HOME=/dafne_home

# Install gosu
COPY docker/install_scripts/* ./
RUN ./install_gosu.sh

COPY --from=compile-image /dload/dafneloader*.whl ./

RUN pip install ./dafneloader*.whl

RUN pip install ipython jupyterlab flit

COPY arena /pytmp/arena
COPY pyproject.arena.toml pytmp/pyproject.toml
  
#The entrypoint should manage permissions here to prevent permission clash.
VOLUME /dafne_home

# Entry trough gosu_entry script, if USER_ID environment variable
# is set (in docker-compose i.e.), that user id will be used

COPY docker/gosu_entry.sh /usr/local/bin/gosu_entry.sh
RUN chmod +x /usr/local/bin/gosu_entry.sh


ENTRYPOINT ["/usr/local/bin/gosu_entry.sh"]

CMD ["jupyter-lab", "--ip=0.0.0.0", "--port=8889", "--no-browser"]
