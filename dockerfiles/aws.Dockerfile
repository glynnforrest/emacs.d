FROM amazonlinux:latest

RUN yum install -y systemd sudo

CMD ["/sbin/init"]
