from django.shortcuts import render
from rest_framework.permissions import IsAdminUser, SAFE_METHODS, BasePermission
from rest_framework import viewsets
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
    queryset = models.Post.objects.all()
    serializer_class = serializers.PostSerializer
