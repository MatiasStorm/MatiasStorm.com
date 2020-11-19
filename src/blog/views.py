from django.shortcuts import render
from rest_framework.permissions import IsAdminUser, SAFE_METHODS, BasePermission
from rest_framework import viewsets
from rest_framework.views import APIView
import datetime
from . import models, serializers

class ReadOnly(BasePermission):
    def has_permission(self, request, view):
        return request.method in SAFE_METHODS

# Create your views here.
class PostCategoryViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAdminUser | ReadOnly]
    queryset = models.PostCategory.objects.all()
    serializer_class = serializers.PostCategorySerializer

class SerieViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAdminUser | ReadOnly]
    queryset = models.Serie.objects.all()
    serializer_class = serializers.SerieSerializer

class PostViewSet(viewsets.ModelViewSet):
    permission_classes = [IsAdminUser | ReadOnly]
    queryset = models.Post.objects.all().order_by("-created")
    serializer_class = serializers.PostSerializer

    def get_queryset(self):
        queryset = self.queryset
        count: int = int( self.request.query_params.get("count", 0) )
        if (count > 0):
            return queryset[0: count]
        return queryset


class StrippedPostViewSet(viewsets.ReadOnlyModelViewSet):
    permission_classes = [IsAdminUser | ReadOnly]
    queryset = models.Post.objects.all().order_by("-created")
    serializer_class = serializers.StrippedPostSerializer

    def get_queryset(self):
        queryset = self.queryset
        after: str = self.request.query_params.get("after", None)
        if after:
            queryset = queryset.filter(created__gt=datetime.datetime.strptime(after, "%Y-%m-%dT%H:%M:%S.%f"))
        before: str = self.request.query_params.get("before", None)
        if before:
            queryset = queryset.filter(created__lt=datetime.datetime.strptime(before, "%Y-%m-%dT%H:%M:%S.%f"))
        count: int = int( self.request.query_params.get("count", 0) )
        if (count > 0):
            return queryset[0: count]
        return queryset

