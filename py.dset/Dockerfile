FROM docker.io/python:3.8-slim-buster AS compile-image

RUN apt-get update
RUN apt-get install -y build-essential git

RUN python -m venv /opt/venv
# Make sure we use the virtualenv:
ENV PATH="/opt/venv/bin:$PATH"

COPY docker/requirements.txt .
RUN pip install -r requirements.txt

# This should be a github repo.
#COPY dafnedset*.whl .
#RUN pip install dafnedset*.whl

#RUN git clone https://github.com/jjclavijo/dafnedset.git &&\
#    cd dafnedset &&\

RUN mkdir /ddset
WORKDIR /ddset

COPY dafnedset/ ./dafnedset
COPY pyproject.toml .
COPY Makefile .

RUN make

RUN pip install /ddset/dafnedset*.whl

#COPY setup.py .
#COPY myapp/ .
#RUN pip install .

FROM docker.io/python:3.8-slim-buster

# ENV DATOS /datos/ -- Data Feeder feeds only data preloaded in DB, no
# raw files needed
ENV SI_BASE sismoident
ENV SI_USER postgres
ENV SI_HOST localhost
ENV SI_PORT 5432
ENV SI_PASS docker
ENV SI_SOCKDIR /sockets/
ENV PGPASSFILE /.pgpass

# Install Postgres client and parallel
COPY docker/install_scripts/* ./
RUN ./install_pp.sh
RUN ./install_gosu.sh

COPY docker/pgpass /.pgpass

COPY --from=compile-image /opt/venv /opt/venv

# Make sure we use the virtualenv:
ENV PATH="/opt/venv/bin:$PATH"

# VOLUME /datos --same as above

# Entry trough gosu_entry script, if USER_ID environment variable
# is set (in docker-compose i.e.), that user id will be used

COPY docker/gosu_entry.sh /usr/local/bin/gosu_entry.sh
RUN chmod +x /usr/local/bin/gosu_entry.sh

ENTRYPOINT ["/usr/local/bin/gosu_entry.sh"]

CMD ["dafne_dataserver"]
