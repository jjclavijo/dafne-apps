FROM python:3.8-slim-buster AS compile-image

RUN apt-get update
RUN apt-get install -y build-essential git

RUN python -m venv /opt/venv
# Make sure we use the virtualenv:
ENV PATH="/opt/venv/bin:$PATH"

RUN mkdir /dcach
WORKDIR /dcach

COPY dafnecacher/ ./dafnecacher
COPY pyproject.toml .
COPY Makefile .

RUN make

FROM tensorflow/tensorflow:2.3.1

# Install gosu
COPY docker/install_scripts/* ./
RUN ./install_gosu.sh

COPY --from=compile-image /dcach/dafnecacher*.whl ./

RUN pip install ./dafnecacher*.whl

# Make sure we use the virtualenv:
ENV PATH="/opt/venv/bin:$PATH"

# Entry trough gosu_entry script, if USER_ID environment variable
# is set (in docker-compose i.e.), that user id will be used

COPY docker/gosu_entry.sh /usr/local/bin/gosu_entry.sh
RUN chmod +x /usr/local/bin/gosu_entry.sh

ENTRYPOINT ["/usr/local/bin/gosu_entry.sh"]

CMD ["dafne_build_cache"]
