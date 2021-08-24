FROM python:3.8

RUN pip install jupyter pipenv

COPY Pipfile Pipfile.lock pyproject.toml setup.cfg /build/
COPY src /build/src
WORKDIR /build/src
RUN make reinstall

COPY demo-notebooks /build/src/demo-notebooks

COPY compose.ipynb .
COPY ensemble.ipynb .
COPY join-h-1.ipynb .
COPY join-h-2.ipynb .
COPY join.ipynb .
COPY seird_hosp.easel .
COPY seirh.easel .
COPY sir-small-beta.easel .
COPY sir.easel .
COPY sirh.easel .
COPY sirs.easel .

EXPOSE 8888
ENTRYPOINT pipenv run jupyter lab --allow-root --no-browser --ip=0.0.0.0 --port=8888
